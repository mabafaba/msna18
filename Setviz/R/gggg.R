#

print(rtools_path() )
has_rtools()
build()
check()
test()
Sys.setenv(PATH = paste("c:\\Rtools\\bin\\", Sys.getenv("PATH"), sep=";"))
Sys.setenv(BINPREF = "C:\\Rtools\\mingw_$(WIN\\bin\\")

