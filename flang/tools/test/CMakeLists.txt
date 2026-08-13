set(FLANG_TOOLS_SOURCE_DIR "${CMAKE_CURRENT_SOURCE_DIR}/..")
set(FLANG_TOOLS_BINARY_DIR "${CMAKE_CURRENT_BINARY_DIR}/..")

llvm_canonicalize_cmake_booleans(
  FLANG_PLUGIN_SUPPORT
  LLVM_INSTALL_TOOLCHAIN_ONLY
)

configure_lit_site_cfg(
  ${CMAKE_CURRENT_SOURCE_DIR}/lit.site.cfg.py.in
  ${CMAKE_CURRENT_BINARY_DIR}/lit.site.cfg.py
  MAIN_CONFIG
  ${CMAKE_CURRENT_SOURCE_DIR}/lit.cfg.py
)

set(FLANG_TOOLS_TEST_DEPS
  # Individual tools we test.
  flang-tidy
)

# Intrinsic module files (iso_c_binding and friends, needed by tests such as
# unused-use.f90) were produced by the 'module_files' target until LLVM 23
# moved builtin .mod generation into the runtimes build.  They are now reached
# through 'flang-rt-test-deps', created in flang/test/CMakeLists.txt -- which
# flang/CMakeLists.txt adds *after* flang/tools, so the target usually does not
# exist yet at this point.  Guard rather than hard-fail configuration; a full
# build produces the module files before the tests are run either way.
set(FLANG_MODULE_DEPS
  flang-rt-test-deps flang-rt-mod
)
foreach(dep ${FLANG_MODULE_DEPS})
  if(TARGET ${dep})
    list(APPEND FLANG_TOOLS_TEST_DEPS ${dep})
  endif()
endforeach()

# Add lit test dependencies.
set(LLVM_UTILS_DEPS
  FileCheck count not
)
foreach(dep ${LLVM_UTILS_DEPS})
  if(TARGET ${dep})
    list(APPEND FLANG_TOOLS_TEST_DEPS ${dep})
  endif()
endforeach()

add_lit_testsuite(check-flang-tools "Running flang-tools/test"
   ${CMAKE_CURRENT_BINARY_DIR}
   DEPENDS ${FLANG_TOOLS_TEST_DEPS}
)

add_lit_testsuites(FLANG-EXTRA ${CMAKE_CURRENT_SOURCE_DIR}
  DEPENDS ${FLANG_TOOLS_TEST_DEPS}
)
