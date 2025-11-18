cmake_minimum_required(VERSION 3.20)
project(ff3 LANGUAGES CXX VERSION 0.1.0)

# Options
option(FF3_BUILD_APPS "Build CLI/benchmark apps" ON)
option(FF3_BUILD_TESTS "Build unit/integration tests" ON)
option(FF3_ENABLE_COVERAGE "Enable coverage flags (GCC/Clang)" OFF)
option(FF3_ENABLE_ASAN "Enable AddressSanitizer in Debug" OFF)
option(FF3_ENABLE_UBSAN "Enable UBSan in Debug" OFF)
option(FF3_ENABLE_LTO "Enable link-time optimization for Release/RelWithDebInfo" OFF)

# Find OpenSSL for AES
find_package(OpenSSL REQUIRED)

# Library target
add_library(ff3 STATIC
    src/core.cpp
    src/api.cpp
    src/alphabets.cpp
)
add_library(ff3::ff3 ALIAS ff3)

target_include_directories(ff3
    PUBLIC
        $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>
        $<INSTALL_INTERFACE:include>
)

target_compile_features(ff3 PUBLIC cxx_std_17)
target_link_libraries(ff3 PUBLIC OpenSSL::Crypto)

# Warnings
if (MSVC)
  target_compile_options(ff3 PRIVATE /W4 /permissive-)
else()
  target_compile_options(ff3 PRIVATE -Wall -Wextra -Wpedantic)
endif()

# Sanitizers
if (FF3_ENABLE_ASAN AND NOT MSVC)
  target_compile_options(ff3 PRIVATE $<$<CONFIG:Debug>:-fsanitize=address>)
  target_link_options(ff3 PRIVATE $<$<CONFIG:Debug>:-fsanitize=address>)
endif()
if (FF3_ENABLE_UBSAN AND NOT MSVC)
  target_compile_options(ff3 PRIVATE $<$<CONFIG:Debug>:-fsanitize=undefined>)
  target_link_options(ff3 PRIVATE $<$<CONFIG:Debug>:-fsanitize=undefined>)
endif()

# Coverage (GCC/Clang)
if (FF3_ENABLE_COVERAGE AND CMAKE_CXX_COMPILER_ID MATCHES "Clang|GNU")
  target_compile_options(ff3 PRIVATE --coverage -O0)
  target_link_options(ff3 PRIVATE --coverage)
endif()

# LTO
include(CheckIPOSupported)
if (FF3_ENABLE_LTO)
  check_ipo_supported(RESULT lto_ok OUTPUT lto_msg)
  if (lto_ok)
    set_property(TARGET ff3 PROPERTY INTERPROCEDURAL_OPTIMIZATION TRUE)
  endif()
endif()

# Fetch nlohmann/json for benchmark JSON output
include(FetchContent)
FetchContent_Declare(
  nlohmann_json
  GIT_REPOSITORY https://github.com/nlohmann/json.git
  GIT_TAG v3.11.3
)
FetchContent_MakeAvailable(nlohmann_json)

# Apps (optional)
if (FF3_BUILD_APPS)
  add_executable(ff3_validate apps/ff3_validate.cpp)
  target_link_libraries(ff3_validate PRIVATE ff3)

  add_executable(ff3_benchmark apps/ff3_benchmark.cpp)
  target_link_libraries(ff3_benchmark PRIVATE ff3 nlohmann_json::nlohmann_json)

  add_executable(ff3_cli apps/ff3_cli.cpp)
  target_link_libraries(ff3_cli PRIVATE ff3)

  add_executable(ff3_stresstest apps/ff3_stresstest.cpp)
  target_link_libraries(ff3_stresstest PRIVATE ff3)
endif()

# Tests (optional, Catch2 via FetchContent)
if (FF3_BUILD_TESTS)
  include(CTest)
  enable_testing()
  FetchContent_Declare(
    catch2
    GIT_REPOSITORY https://github.com/catchorg/Catch2.git
    GIT_TAG v3.6.0
  )
  FetchContent_MakeAvailable(catch2)

  # Only build tests if test files exist
  set(TEST_SOURCES)
  foreach(test_file test_core.cpp test_api.cpp test_alphabets.cpp test_nist.cpp)
    if(EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/tests/${test_file}")
      list(APPEND TEST_SOURCES "tests/${test_file}")
    endif()
  endforeach()

  if(TEST_SOURCES)
    add_executable(ff3_tests ${TEST_SOURCES})
    target_link_libraries(ff3_tests PRIVATE ff3 Catch2::Catch2WithMain)
    target_compile_definitions(ff3_tests PRIVATE FF3_TEST_DATA_DIR="${CMAKE_CURRENT_SOURCE_DIR}/tests/data")

    include(Catch)
    catch_discover_tests(ff3_tests)
  endif()
endif()

# Install (library + headers + CMake package)
include(GNUInstallDirs)
install(TARGETS ff3
  EXPORT ff3Targets
  ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
  LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
  RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
)
install(DIRECTORY include/ DESTINATION ${CMAKE_INSTALL_INCLUDEDIR})
install(EXPORT ff3Targets
  FILE ff3Targets.cmake
  NAMESPACE ff3::
  DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/ff3
)
include(CMakePackageConfigHelpers)
write_basic_package_version_file(
  "${CMAKE_CURRENT_BINARY_DIR}/ff3ConfigVersion.cmake"
  VERSION ${PROJECT_VERSION}
  COMPATIBILITY SameMajorVersion
)
if(EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/cmake/ff3Config.cmake.in")
  configure_package_config_file(
    "${CMAKE_CURRENT_SOURCE_DIR}/cmake/ff3Config.cmake.in"
    "${CMAKE_CURRENT_BINARY_DIR}/ff3Config.cmake"
    INSTALL_DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/ff3
  )
  install(FILES
    "${CMAKE_CURRENT_BINARY_DIR}/ff3Config.cmake"
    "${CMAKE_CURRENT_BINARY_DIR}/ff3ConfigVersion.cmake"
    DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/ff3
  )
endif()