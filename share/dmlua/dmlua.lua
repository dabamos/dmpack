-- dmlua.lua
--
-- Basic Lua script for dmlua(1): The function `process()` prints the passed
-- observation table `observ` to standard output, then returns it unmodified.
--
function process(observ)
    print(dump(observ))
    return observ
end

function dump(o)
   if type(o) == 'table' then
      local s = '{ '
      for k, v in pairs(o) do
         if type(k) ~= 'number' then k = '"' .. k .. '"' end
         s = s .. '[' .. k .. '] = ' .. dump(v) .. ','
      end
      return s .. '} '
   else
      return tostring(o)
   end
end
