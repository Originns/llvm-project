# Test runner infrastructure for Flang-based tools. This configures the Flang
# test trees for use by Lit, and delegates to LLVM's lit test handlers.
#
# Note that currently we don't support stand-alone builds of Flang; you must
# be building Flang from within a combined LLVM+Flang checkout.

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
