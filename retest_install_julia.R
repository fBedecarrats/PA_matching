install.packages("JuliaCall")

library(JuliaCall)

install_julia()
julia_setup()
julia_install_package("ArchGDAL")
julia_library("libgeotiff_jll")
julia_update_package("libgeotiff_jll")
julia_library("libgeotiff_jll")
julia_install_package("LibCURL")
julia_eval('Pkg.build("libgeotiff_jll")')
julia_eval('Pkg.build("GDAL_jll")')
julia_eval('using libgeotiff_jll')
