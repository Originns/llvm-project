set(LLVM_LINK_COMPONENTS
  FrontendOpenACC
  FrontendOpenMP
  Support
  )

add_flang_tool(flang-tidy
  FlangTidy.cpp
  bugprone/ArithmeticGotoCheck.cpp
  bugprone/ImplicitDeclCheck.cpp
  bugprone/UnusedIntentCheck.cpp
  modernize/AvoidCommonBlocks.cpp
  tool/FlangTidyMain.cpp
  tool/FlangTidyToolMain.cpp
  utils/UnwrapSymbol.cpp
)

llvm_update_compile_flags(flang-tidy)

target_link_libraries(flang-tidy
  PRIVATE
  FortranCommon
  FortranParser
  FortranSemantics
  )

