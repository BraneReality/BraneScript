set(VCPKG_TARGET_ARCHITECTURE x64)
set(VCPKG_CRT_LINKAGE static) # Or dynamic if you prefer
set(VCPKG_LIBRARY_LINKAGE static)
set(VCPKG_CHAINLOAD_TOOLCHAIN_FILE "${CMAKE_CURRENT_LIST_DIR}/../toolchains/toolchain-zig-windows.cmake")
set(VCPKG_CMAKE_SYSTEM_NAME Windows)
set(VCPKG_ENV_PASSTHROUGH PATH)