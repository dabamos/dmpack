#!/usr/bin/env lua54

local f = io.open("libfortran.so", "r")

if f ~= nil then
    io.close(f)
else
    print("Copy shared library libfortran.so to this directory first.")
    os.exit()
end

-- import shared library `libfortran.so`
require("libfortran")

-- call `hello()` in `libfortran.so`
hello()
