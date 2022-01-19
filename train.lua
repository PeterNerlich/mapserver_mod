
local advtrains_present = minetest.get_modpath("advtrains") and true or false
local last_set_by = {}
local markers = {}

local find_neighbor_blocks -- defined later
local update_neighbors --defined later
local recalculate_line_to -- defined later
local visualize_to_player -- defined later
local TRAVERSER_LIMIT = 1000

local update_formspec = function(meta)
	local line = meta:get_string("line")
	local station = meta:get_string("station")
	local index = meta:get_string("index")
	local color = meta:get_string("color") or ""
	local rail_pos = meta:get_string("rail_pos") or ""

	local rail_btns = ""
	if advtrains_present then
		if rail_pos == "" then
			rail_btns = "button_exit[4,3.5;2.5,1;set_rail_pos;Set rail]"
		else
			rail_btns = "button_exit[4,3.5;2.5,1;set_rail_pos;" .. rail_pos .. "]" ..
				"button[6.5,3.5;1.5,1;clear_rail_pos;Clear rail]"
		end
	end

	local prv = meta:get_string("prv_pos")
	local path = meta:get_string("linepath_from_prv")
	local nxt = meta:get_string("nxt_pos")

	meta:set_string("infotext", "Train: Line=" .. line .. ", Station=" .. station ..
		(prv ~= "" and (", prv="..prv) or "") ..
		(path ~= "" and " (found line)" or "") ..
		(nxt ~= "" and (", nxt="..nxt) or "") ..
		(line ~= "" and prv == "" and nxt == "" and (", no neighbors found") or ""))

	meta:set_string("formspec", "size[8,4;]" ..
		-- col 1
		"field[0,1;4,1;line;Line;" .. line .. "]" ..
		"button_exit[4,1;4,1;save;Save]" ..

		-- col 2
		"field[0,2.5;4,1;station;Station;" .. station .. "]" ..
		"field[4,2.5;4,1;index;Index;" .. index .. "]" ..

		-- col 3
		"field[0,3.5;4,1;color;Color;" .. color .. "]" ..
		rail_btns
	)

end


minetest.register_node("mapserver:train", {
	description = "Mapserver Train",
	tiles = {
		"mapserver_train.png"
	},
	groups = {cracky=3,oddly_breakable_by_hand=3},
	sounds = moditems.sound_glass(),
	can_dig = mapserver.can_interact,

	after_place_node = function(pos, placer, itemstack, pointed_thing)
		local meta = minetest.get_meta(pos)

		local last_index = 0
		local last_line = ""
		local last_color = ""

		if minetest.is_player(placer) then
			local name = placer:get_player_name()
			if name ~= nil then
				name = string.lower(name)
				if last_set_by[name] ~= nil then
					last_index = last_set_by[name].index + 5
					last_line = last_set_by[name].line
					last_color = last_set_by[name].color
				else
					last_set_by[name] = {}
				end

				last_set_by[name].index = last_index
				last_set_by[name].line = last_line
				last_set_by[name].color = last_color
			end
		end

		meta:set_string("station", "")
		meta:set_string("line", last_line)
		meta:set_int("index", last_index)
		meta:set_string("color", last_color)
		meta:set_string("rail_pos", "")

		update_neighbors(pos, meta, placer, true)

		return mapserver.after_place_node(pos, placer, itemstack, pointed_thing)
	end,

	after_dig_node = function(pos, oldnode, oldmetadata, player)
		local fake_meta = minetest.get_meta(pos)

		-- TODO: why doesn't this work properly?

		update_neighbors(pos, fake_meta, player, true)
	end,

	on_punch = function(pos, node, puncher, pointed_thing)
		if minetest.is_player(puncher) and not mapserver.can_interact(pos, puncher) then
			return
		end

		local meta = minetest.get_meta(pos)

		update_neighbors(pos, meta, puncher, true)
	end,

	on_receive_fields = function(pos, formname, fields, sender)

		if not mapserver.can_interact(pos, sender) then
			return
		end

		local meta = minetest.get_meta(pos)
		local name = sender:get_player_name()
		local lname = string.lower(name)

		if fields.save then
			if last_set_by[lname] == nil then
				last_set_by[lname] = {}
			end

			local index = tonumber(fields.index)
			if index ~= nil then
				index = index
			end

			meta:set_string("color", fields.color)
			meta:set_string("line", fields.line)
			meta:set_string("station", fields.station)
			meta:set_int("index", index)

			last_set_by[lname].color = fields.color
			last_set_by[lname].line = fields.line
			last_set_by[lname].station = fields.station
			last_set_by[lname].index = index

			update_neighbors(pos, meta, sender, true)

		elseif fields.clear_rail_pos then
			meta:set_string("rail_pos", "")
			update_neighbors(pos, meta, sender, true)

		elseif fields.set_rail_pos then
			minetest.chat_send_player(name, "Please punch the nearest rail this train line follows.")
			if last_set_by[lname] == nil then
				last_set_by[lname] = {}
			end
			last_set_by[lname].waiting_for_rail = pos
		end
	end
})

minetest.register_on_punchnode(function(pos, node, sender, pointed_thing)
	local name = sender:get_player_name()
	local lname = string.lower(name)
	local blockpos = nil
	if last_set_by[lname] ~= nil and
		last_set_by[lname].waiting_for_rail ~= nil then

		blockpos = last_set_by[lname].waiting_for_rail
	else
		return
	end
	if not mapserver.can_interact(blockpos, sender) then
		return
	end

	if blockpos and advtrains_present then
		if vector.distance(pos, blockpos) <= 20 then
			local node_ok, conns, rhe = advtrains.get_rail_info_at(pos, advtrains.all_tracktypes)
			if node_ok then
				local meta = minetest.get_meta(blockpos)
				meta:set_string("rail_pos", minetest.pos_to_string(pos))
				update_neighbors(blockpos, meta, sender, true)
			else
				minetest.chat_send_player(name, "This is not a rail! Aborted.")
			end
		else
			minetest.chat_send_player(name, "Node is too far away. Aborted.")
		end
		last_set_by[lname].waiting_for_rail = nil
	end
end)

if mapserver.enable_crafting then
	minetest.register_craft({
	    output = 'mapserver:train',
	    recipe = {
				{"", moditems.steel_ingot, ""},
				{moditems.paper, moditems.goldblock, moditems.paper},
				{"", moditems.glass, ""}
			}
	})
end


update_neighbors = function(pos, meta, player, update_markers)
	update_markers = update_markers and true or false
	if meta == nil then
		meta = minetest.get_meta(pos)
	end
	local name = player and minetest.is_player(player) and player:get_player_name() or nil

	local line = meta:get_string("line")
	local index = tonumber(meta:get_string("index"))
	local rail_pos = meta:get_string("rail_pos")

	-- if anything critical changed (pos/line/index) virtually remove us
	local prv = minetest.string_to_pos(meta:get_string("prv_pos"))
	local nxt = minetest.string_to_pos(meta:get_string("nxt_pos"))
	local prv_meta = prv ~= nil and minetest.get_meta(prv) or nil
	local nxt_meta = nxt ~= nil and minetest.get_meta(nxt) or nil

	if prv ~= nil and prv_meta:get_string("line") ~= line and
		nxt ~= nil and nxt_meta:get_string("line") ~= line then
		if prv ~= nil and nxt == nil then
			-- loose end
			prv_meta:set_string("nxt_pos", "")
			prv_meta:set_string("nxt_index", "")
			prv_meta:set_string("nxt_rail_pos", "")
		elseif prv == nil and nxt ~= nil then
			-- loose end
			nxt_meta:set_string("prv_pos", "")
			nxt_meta:set_string("prv_index", "")
			nxt_meta:set_string("prv_rail_pos", "")

			nxt_meta:set_string("linepath_from_prv", "")
		else
			-- we were in the middle
			prv_meta:set_string("nxt_pos", nxt)
			prv_meta:set_string("nxt_index", meta:get_string("nxt_index"))
			prv_meta:set_string("nxt_rail_pos", meta:get_string("nxt_rail_pos"))

			nxt_meta:set_string("prv_pos", prv)
			nxt_meta:set_string("prv_index", meta:get_string("prv_index"))
			nxt_meta:set_string("prv_rail_pos", meta:get_string("prv_rail_pos"))

			recalculate_line_to(prv, nxt, prv_meta, nxt_meta)
		end

		for _,m in ipairs({prv_meta, nxt_meta}) do
			if m ~= nil then
				update_formspec(m)
			end
		end

		-- remove meta from self
		meta:set_string("prv_pos", "")
		meta:set_string("prv_index", "")
		meta:set_string("prv_rail_pos", "")

		meta:set_string("nxt_pos", "")
		meta:set_string("nxt_index", "")
		meta:set_string("nxt_rail_pos", "")

		meta:set_string("linepath_from_prv", "")
	end

	-- update or add us
	-- repurposing prv, prv_meta etc. vars
	local neighbors = find_neighbor_blocks(pos, meta, name)
	prv = neighbors[1]
	nxt = neighbors[2]
	prv_meta = prv ~= nil and minetest.get_meta(prv.pos) or nil
	nxt_meta = nxt ~= nil and minetest.get_meta(nxt.pos) or nil
	-- if index or rail pos changed, recalculate line path
	if prv ~= nil then
		local old_nxt_pos = prv_meta:get_string("nxt_pos")
		local old_nxt_index = tonumber(prv_meta:get_string("nxt_index"))
		local old_nxt_rail_pos = prv_meta:get_string("nxt_rail_pos")

		-- if old info on prev does not match us, set correct
		if old_nxt_pos ~= (nxt == nil and "" or nxt.pos) then
			if old_nxt_pos == pos then
				-- phew, it's just us
			elseif nxt ~= nil and old_nxt_pos == nxt.pos then
				-- okay we are just freshly added
				-- update the previous block
				prv_meta:set_string("nxt_pos", minetest.pos_to_string(pos))
			else
				-- there are more nodes we don't know about!
			end
		end
		if old_nxt_index ~= index then
			-- index changed! since our position is still unchanged
			-- (otherwise removing/re-adding above would have happened instead)
			-- we just need to update the info, without linepath recalculation
			prv_meta:set_int("nxt_index", index)
		end
		if old_nxt_rail_pos ~= rail_pos then
			-- rail pos changed! definitely need linepath recalculation
			prv_meta:set_string("nxt_rail_pos", rail_pos)
			meta:set_string("linepath_from_prv", "")
		end

		meta:set_string("prv_pos", minetest.pos_to_string(prv.pos))
		meta:set_int("prv_index", prv.index)
		meta:set_string("prv_rail_pos", prv.rail_pos)
	end
	if nxt ~= nil then
		local old_prv_pos = nxt_meta:get_string("prv_pos")
		local old_prv_index = tonumber(nxt_meta:get_string("prv_index"))
		local old_prv_rail_pos = nxt_meta:get_string("prv_rail_pos")

		-- if old info on next does not match us, set correct
		if old_prv_pos ~= (prv == nil and "" or prv.pos) then
			if old_prv_pos == pos then
				-- phew, it's just us
			elseif prv ~= nil and old_prv_pos == prv.pos then
				-- okay we are just freshly added
				-- update the previous block
				nxt_meta:set_string("prv_pos", minetest.pos_to_string(pos))
				nxt_meta:set_string("linepath_from_prv", "")
			else
				-- there are more nodes we don't know about!
			end
		end
		if old_prv_index ~= index then
			-- index changed! since our position is still unchanged
			-- (otherwise removing/re-adding above would have happened instead)
			-- we just need to update the info, without linepath recalculation
			nxt_meta:set_int("prv_index", index)
		end
		if old_prv_rail_pos ~= rail_pos then
			-- rail pos changed! definitely need linepath recalculation
			nxt_meta:set_string("prv_rail_pos", rail_pos)
			nxt_meta:set_string("linepath_from_prv", "")
		end

		meta:set_string("nxt_pos", minetest.pos_to_string(nxt.pos))
		meta:set_int("nxt_index", nxt.index)
		meta:set_string("nxt_rail_pos", nxt.rail_pos)
	end

	if rail_pos ~= "" then
		if prv ~= nil and prv.rail_pos ~= "" then
			local line = recalculate_line_to(prv.pos, pos, prv_meta, meta)
			if name then
				if #line > 0 then
					minetest.chat_send_player(name, "Found line from prv ("..tonumber(#line).."): "..table.concat(line, "->"))
				else
					minetest.chat_send_player(name, "Did not find line from prv.")
				end
			end
		end
		if nxt ~= nil and nxt.rail_pos ~= "" then
			local line = recalculate_line_to(pos, nxt.pos, meta, nxt_meta)
			if name then
				if #line > 0 then
					minetest.chat_send_player(name, "Found line to nxt ("..tonumber(#line).."): "..table.concat(line, "->"))
				else
					minetest.chat_send_player(name, "Did not find line to nxt.")
				end
			end
		end
	end

	for _,m in ipairs({prv_meta, nxt_meta}) do
		if m ~= nil then
			update_formspec(m)
		end
	end
	update_formspec(meta)

	if update_markers and
		player and minetest.is_player(player) then
		visualize_to_player(pos, meta, neighbors, player, true)
	end
end

local nroot = function(root, num)
	return num^(1/root)
end

-- Searching an area for nodes is expensive.
-- Minetest limits the amount to 4,096,000 nodes.
-- Because there is not a good way to form one cuboid to fit all major long-distance usecases
-- and this will not be frequently executed on a server (only every time a player manually
-- sets or updates a train map block) we take all we can with 3 separate ranges:
-- - One layer for most applications in long, flat stretches, allowing for 3 nodes of up/down
--   deviation, maxes out on 381 x and z deviation.
-- - One smaller, but higher cuboid on top and bottom of it each, stretching 123 in every
--   x and z direction and 67 up/down
-- This should be very luxurious and prove enough for almost everything.
local max_nodes = 4096000
local cuboid_width_for_height = function(height)
	return math.floor(math.sqrt(max_nodes / height))
end
local span_rectangle = function(pos, radius, height, v_offset, v_invert)
	local v_dir = v_invert and -1 or 1
	return { vector.add(pos, vector.multiply(vector.new(-radius, v_offset, -radius), v_dir)),
			 vector.add(pos, vector.multiply(vector.new(radius, height+v_offset, radius), v_dir)) }
end
local halve_area = function(length)
	return math.floor((length-1) / 2)
end
local twocube_length = math.floor(nroot(3, max_nodes*2))
local flat_height = 7
local flat_length = cuboid_width_for_height(flat_height)
local cuboid_height = math.floor(twocube_length/3)
local cuboid_length = cuboid_width_for_height(cuboid_height)
local area_from_offset = function(pos, offset)
	return {vector.subtract(pos, offset), vector.add(pos, offset)}
end
local get_volume = function(span)
	local diff = vector.subtract(span[2], span[1])
	return (math.abs(diff.x)+1) * (math.abs(diff.y)+1) * (math.abs(diff.z)+1)
end

find_neighbor_blocks = function(pos, meta, name)
	if meta == nil then
		meta = minetest.get_meta(pos)
	end
	local all = {}
	local line = meta:get_string("line")
	local index = tonumber(meta:get_string("index"))
	local rail_pos = meta:get_string("rail_pos")

	local empty = {nil, nil, {
		{ pos = pos, meta = meta,
		line = line, index = index, rail_pos = rail_pos }
	}}
	if line == "" then
		return empty
	end

	-- the offsets are chosen so that the resulting area is just under the maximum allowable size
	local areas = {
		flat = area_from_offset(pos, vector.new(halve_area(flat_length), halve_area(flat_height), halve_area(flat_length))),
		upper_half = span_rectangle(pos, halve_area(cuboid_length), cuboid_height-1, halve_area(flat_height)+1),
		lower_half = span_rectangle(pos, halve_area(cuboid_length), cuboid_height-1, halve_area(flat_height)+1, true)
	}
	local blocks = {}
	for i,span in pairs(areas) do
		if get_volume(span) > max_nodes then
			minetest.chat_send_player(name, "Internal Error searching for nearby nodes: Invalid span "..i.." between "..minetest.pos_to_string(span[1]).." and "..minetest.pos_to_string(span[2]).." (volume of "..tostring(get_volume(span))..")")
			minetest.log("error", "[mapserver_mod][trainlines] Internal Error searching for nearby nodes: Invalid span "..i.." between "..minetest.pos_to_string(span[1]).." and "..minetest.pos_to_string(span[2]).." (volume of "..tostring(get_volume(span))..")")
			return empty
		end
		blocks[i] = minetest.find_nodes_in_area(span[1], span[2], "mapserver:train")
	end
	local prv = nil
	local nxt = nil
	local m = nil

	for _,span in pairs(blocks) do
		for _,p in pairs(span) do
			m = minetest.get_meta(p)
			if m:get_string("line") == line then
				local idx = tonumber(m:get_string("index"))
				local rail = m:get_string("rail_pos")
				if idx < index and
					(prv == nil or idx > prv.index) then
						prv = {
							pos = p,
							index = idx,
							rail_pos = rail
						}
				end
				if idx > index and
					(nxt == nil or idx < nxt.index) then
						nxt = {
							pos = p,
							index = idx,
							rail_pos = rail
						}
				end
				table.insert(all, {
					pos = p, meta = m,
					line = line, index = idx, rail_pos = rail
				})
			end
		end
	end

	return {prv, nxt, all}
end

local clone = nil
clone = function(tbl, n)
	local out = {}
	local i,v = next(tbl, nil)
	while i do
		if type(v) == "table" then
			out[i] = clone(v, (n or 0)+1)
		else
			out[i] = v
		end
		i,v = next(tbl, i)
	end
	return out
end

recalculate_line_to = function(pos_a, pos_b, meta_a, meta_b)
	if meta_a == nil then
		meta_a = minetest.get_meta(pos_a)
	end
	if meta_b == nil then
		meta_b = minetest.get_meta(pos_b)
	end
	local line = {}
	local rail_pos_a = minetest.string_to_pos(meta_a:get_string("rail_pos"))
	local rail_pos_b = minetest.string_to_pos(meta_b:get_string("rail_pos"))
	local node_ok_a, conns_a, rhe_a, node_ok_b, conns_b, rhe_b
	if rail_pos_a then
		node_ok_a, conns_a, rhe_a = advtrains.get_rail_info_at(rail_pos_a, advtrains.all_tracktypes)
		if rail_pos_b then
			node_ok_b, conns_b, rhe_b = advtrains.get_rail_info_at(rail_pos_b, advtrains.all_tracktypes)
		end
	end
	if not node_ok_a or not node_ok_b then
		table.insert(line, node_ok_a and minetest.pos_to_string(rail_pos_a) or minetest.pos_to_string(pos_a))
	else
		-- depth first search for rail_pos_b,
		-- vector.distance(step, rail_pos_b) is score

		-- keep track of all visited positions to avoid going in circles
		local visited_nodes = {}
		-- heads of search positions: {pos=<pos>, score=<cached score>, steps=<nth node tried>, line=<line until pos>}
		local progress = {}

		-- put starting rail in, for every direction
		for connid, conn in ipairs(conns_a) do
			table.insert(progress, {
				pos = rail_pos_a,
				conns = conns_a,
				connid = connid,
				steps = 0,
				score = vector.distance(rail_pos_a, rail_pos_b),
				line = {}
			})
		end

		while next(progress, nil) do
			local min_idx = nil
			local min_item = nil
			-- try the node closest to the destination
			for i,v in pairs(progress) do
				if v.steps < TRAVERSER_LIMIT and
					(min_item == nil or v.score < min_item.score) then
					min_idx = i
					min_item = v
				end
			end

			-- check the adjacent rail
			local adj_pos, adj_connid, conn_idx, nextrail_y, next_conns = advtrains.get_adjacent_rail(min_item.pos, min_item.conns, min_item.connid, advtrains.all_tracktypes)
			if not adj_pos then
				-- there is no rail, end-of-track
				progress[min_idx] = nil
			elseif visited_nodes[minetest.pos_to_string(adj_pos)..adj_connid] ~= nil then
				-- already been here in this direction, no use repeating same steps
				progress[min_idx] = nil
			elseif minetest.pos_to_string(adj_pos) == minetest.pos_to_string(rail_pos_b) then
				-- found destination!
				-- set line and break loop
				line = min_item.line
				table.insert(line, minetest.pos_to_string(rail_pos_b))
				break
			else
				-- remember we did this one to prevent circles
				visited_nodes[minetest.pos_to_string(adj_pos)..adj_connid] = true

				if min_item.steps > TRAVERSER_LIMIT then
					print("went over traverser limit! "..minetest.pos_to_string(rail_pos_a).." → "..minetest.pos_to_string(adj_pos))
				else
					local inconn = next_conns[adj_connid]
					-- query the next conns
					local deg45 = AT_CMAX/8
					for nconnid, nconn in ipairs(next_conns) do
						local normed = (nconn.c-inconn.c)%AT_CMAX
						-- only accept conns that turn 90deg at most
						if normed >= deg45 and normed <= AT_CMAX-deg45 then
							local line = clone(min_item.line)
							if normed ~= AT_CMAX/2 then
								table.insert(line, minetest.pos_to_string(adj_pos))
							end
							table.insert(progress, {
								pos = adj_pos,
								conns = next_conns,
								connid = nconnid,
								steps = min_item.steps + 1,
								score = vector.distance(adj_pos, rail_pos_b),
								line = line
							})
						end
					end
				end
				-- we are done with this item
				progress[min_idx] = nil
			end
		end
	end
	meta_b:set_string("linepath_from_prv", table.concat(line, ";"))
	return line
end

-- show found markers when saving object

-- when clicky save, show markers for found blocks
-- color according to index
-- add NO RAIL SET if no rail_pos
-- add markers for path found?
-- remove markers after minute

local ifilter = function(tbl, fn)
	local out = {}
	for i,v in ipairs(tbl) do
		if fn(v, i, tbl) then
			table.insert(out)
		end
	end
	return out
end
local filter = function(tbl, fn)
	local out = {}
	for k,v in pairs(tbl) do
		if fn(v, k, tbl) then
			out[k] = v
		end
	end
	return out
end
local map = function(tbl, fn)
	local out = {}
	for k,v in pairs(tbl) do
		out[k] = fn(v, k, tbl)
	end
	return out
end

local find_all = function(str, sub)
	local found = 0
	local positions = {}
	while(found)do
		found = found + 1
		found = str:find(sub, found)
		table.insert(positions, found)
	end
	return positions
end

local split = function(str, sep)
	local seps = find_all(str, sep)
	local e = 1
	local out = {}
	for k,v in pairs(seps) do
		table.insert(out, string.sub(str, e, v-1))
		e = v + #sep
	end
	table.insert(out, string.sub(str, e, #str))
	return out
end

local interpolate = function(a, b, t)
	return t*(b-a) + a
end

local RGB = function(r, g, b)
	-- we have 255 steps per subpixel
	local base = 0xFF

	-- clamp values, discard fractions
	r = math.floor(base * math.max(0, math.min(1, r)))
	g = math.floor(base * math.max(0, math.min(1, g)))
	b = math.floor(base * math.max(0, math.min(1, b)))

	return r*2^16 + g*2^8 + b
end

local c_this_path = RGB(.8,.9,.8)
local c_next_path = RGB(.7,.7,.8)
local colormethis = function(v, extremes, halfway_point, neighbors)
	if v.index == nil or v.duplicate_index then
		return RGB(1,0,1)
	elseif v.index == halfway_point then
		return RGB(1,1,1)
	end

	local a = extremes[1]
	local b = extremes[2]
	if v.index < halfway_point then
		if neighbors ~= nil and neighbors[1] ~= nil then
			b = neighbors[1].index
		else
			b = halfway_point
		end
	else
		if neighbors ~= nil and neighbors[2] ~= nil then
			a = neighbors[2].index
		else
			a = halfway_point
		end
	end
	local t
	if b - a == 0 then
		t = 1
	else
		t = (v.index - a) / (b - a)
	end

	-- helper variables
	local t2 = t^2	-- t squared
	local it = 1 - t	-- inverse t
	local it2 = it^2	-- inverse t squared

	local base = .5
	local r = base
	local g = base
	local b = base

	if v.index < halfway_point then
		--g = g + t2 * (1-base)
		g = 1
		t = t2
	else
		--b = b + it2 * (1-base)
		b = 1
		g = interpolate(g, 1, .4)
		t = it2
	end

	local avg = math.sqrt(r^2 + g^2 + b^2)
	r = interpolate(avg/3*2, r, t)
	g = interpolate(avg/3*2, g, t)
	b = interpolate(avg/3*2, b, t)

	if v.duplicate_index then
		return RGB(1,0,1)
	elseif not v.has_rail then
		r = 1
	elseif not v.has_path then
		r = interpolate(r, 1, .8)
	end

	return RGB(r,g,b)
end

local displayindex = function(v, line)
	local out = line..": ["..tostring(v.index).."]"
	if v.index == nil then
		out = out .. " INVALID INDEX"
	elseif v.duplicate_index then
		out = out .. " DUPLICATE INDEX"
	elseif not v.has_rail then
		out = out .. " (no rail)"
	elseif not v.has_path then
		out = out .. " (no path)"
	end
	return out
end

visualize_to_player = function(pos, meta, neighbors, player, show_path)
	local name = player:get_player_name()
	local index = tonumber(meta:get_string("index"))
	local line = meta:get_string("line")

	local found_blocks = neighbors[3]
	if markers[name] == nil then
		markers[name] = {}
	end

	table.sort(found_blocks, function(a,b) return a.index < b.index end)
	local idx_extremes = {found_blocks[1].index, found_blocks[#found_blocks].index}

	local new_markers = {}
	local added_markers = {}
	local changed_markers = {}

	local done = {}
	local last_idx = nil
	for i,v in ipairs(found_blocks) do
		v.path = v.meta:get_string("linepath_from_prv")
		v.has_rail = v.rail_pos ~= ""
		v.has_path = v.path ~= ""
		v.duplicate_index = v.index ~= nil and v.index == last_idx
		if v.duplicate_index then
			found_blocks[i-1].duplicate_index = true
		end
		last_idx = v.index

		v.meta = nil
	end
	for i,v in ipairs(found_blocks) do
		local pos_string = minetest.pos_to_string(v.pos)
		done[pos_string] = v
		if true then
		--if markers[name][pos_string] == nil then
			v.hud_id = player:hud_add({
				hud_elem_type = "waypoint",
				name = displayindex(v, line),
				text = "",
				number = colormethis(v, idx_extremes, index, neighbors),
				world_pos = v.pos
			})

			--added_markers = added_markers + 1
			table.insert(added_markers, {pos_string, v.hud_id})
		else
			v.hud_id = markers[name][pos_string].hud_id
			player:hud_change(v.hud_id, "name", displayindex(v, line))
			player:hud_change(v.hud_id, "number", colormethis(v, idx_extremes, index, neighbors))

			--changed_markers = changed_markers + 1
			table.insert(changed_markers, {pos_string, v.hud_id})
		end
		--new_markers = new_markers + 1
		table.insert(new_markers, {pos_string, v.hud_id})
	end

	if show_path then
		local pos_string = minetest.pos_to_string(pos)
		if done[pos_string].has_path then
			local path = split(done[pos_string].path, ";")
			for _,p in ipairs(path) do
				done[p] = {pos = minetest.string_to_pos(p)}
				done[p].pos.y = done[p].pos.y - .4
				if true then
				--if markers[name][p] == nil then
					done[p].hud_id = player:hud_add({
						hud_elem_type = "waypoint",
						name = "", --"(pPath) | "..tostring(c_this_path),
						text = "",
						number = c_this_path,
						world_pos = done[p].pos
					})

					--added_markers = added_markers + 1
					table.insert(added_markers, {p, done[p].hud_id})
				else
					done[p].hud_id = markers[name][pos_string].hud_id
					player:hud_change(done[p].hud_id, "name", "") --"(pPath) | "..tostring(c_this_path))
					player:hud_change(done[p].hud_id, "number", c_this_path)

					--changed_markers = changed_markers + 1
					table.insert(changed_markers, {p, done[p].hud_id})
				end
				--new_markers = new_markers + 1
				table.insert(new_markers, {p, done[p].hud_id})
			end
		end

		if neighbors[2] ~= nil then
			pos_string = minetest.pos_to_string(neighbors[2].pos)
			if done[pos_string].has_path then
				local path = split(done[pos_string].path, ";")
				for _,p in ipairs(path) do
					done[p] = {pos = minetest.string_to_pos(p)}
					if true then
					--if markers[name][p] == nil then
						done[p].hud_id = player:hud_add({
							hud_elem_type = "waypoint",
							name = "", --"(nPath) | "..tostring(c_next_path),
							text = "",
							number = c_next_path,
							world_pos = done[p].pos
						})

						--added_markers = added_markers + 1
						table.insert(added_markers, {p, done[p].hud_id})
					else
						done[p].hud_id = markers[name][pos_string].hud_id
						player:hud_change(done[p].hud_id, "name", "") --"(nPath) | "..tostring(c_next_path))
						player:hud_change(done[p].hud_id, "number", c_next_path)

						--changed_markers = changed_markers + 1
						table.insert(changed_markers, {p, done[p].hud_id})
					end
					--new_markers = new_markers + 1
					table.insert(new_markers, {p, done[p].hud_id})
				end
			end
		end
	end

	local prev_markers = {}
	local deleted_markers = {}
	-- clear vanished elements
	for p,v in pairs(markers[name]) do
		--if done[p] == nil then
			player:hud_remove(v.hud_id)
			--deleted_markers = deleted_markers + 1
			table.insert(deleted_markers, {p, v.hud_id})
		--end
		--prev_markers = prev_markers + 1
		table.insert(prev_markers, {p, v.hud_id})
	end

	--[[
	for p,v in pairs(markers[name]) do
		print("old: "..p..": "..v.hud_id)
	end
	]]--
	markers[name] = done
	--[[
	for p,v in pairs(markers[name]) do
		print("new: "..p..": "..v.hud_id)
	end

	local pme = function(t)
		t = map(t, function(v) return v[1].." ["..tostring(v[2]).."]" end)
		table.sort(t)
		return "{ "..table.concat(t, ", ").." }"
	end

	minetest.log("error", tostring(#new_markers).." new / "..tostring(#prev_markers).." previous markers" ..
		" ("..tostring(#added_markers).." added, "..tostring(#changed_markers).." changed, "..tostring(#deleted_markers).." deleted)")
	minetest.log("error", "new:      "..pme(new_markers))
	minetest.log("error", "previous: "..pme(prev_markers))
	minetest.log("error", "added:    "..pme(added_markers))
	minetest.log("error", "changed:  "..pme(changed_markers))
	minetest.log("error", "deleted:  "..pme(deleted_markers))
	]]--
end
