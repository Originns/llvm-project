set(LLVM_LINK_COMPONENTS
  FrontendOpenACC
  FrontendOpenMP
  Support
  )

add_flang_tool(flang-tidy
  bugprone/ArithmeticGotoCheck.cpp
  bugprone/ImplicitDeclCheck.cpp
  bugprone/PrecisionLossCheck.cpp
  bugprone/ShortCircuitCheck.cpp
  bugprone/UndeclaredProcCheck.cpp
  bugprone/UninitializedVarCheck.cpp
  bugprone/UnusedIntentCheck.cpp
  FlangTidy.cpp
  modernize/AvoidCommonBlocks.cpp
  tool/FlangTidyMain.cpp
  tool/FlangTidyToolMain.cpp
)

llvm_update_compile_flags(flang-tidy)

target_link_libraries(flang-tidy
  PRIVATE
  FortranCommon
  FortranEvaluate
  FortranParser
  FortranSemantics
  )
