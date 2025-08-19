#!/usr/bin/env lua54

local f = io.open("fortran.so", "r")

if f ~= nil then
    io.close(f)
else
    print("Copy shared library fortran.so to this directory first.")
    os.exit()
end

-- import shared library `fortran.so`
require("fortran")

-- call `hello()` in `fortran.so`
hello()
