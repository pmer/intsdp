cmake_minimum_required(VERSION 2.8.9...3.19)
project(intspd C CXX)

set(UTIL_ONLY 1)
add_subdirectory(Tsunagari)

set(EXE intspd)

get_filename_component(HERE "${CMAKE_CURRENT_LIST_FILE}" PATH)

add_executable(${EXE}
    ${HERE}/src/main.cpp
)

include_directories(${TSUNAGARI_INCLUDE_DIRECTORIES} src)
target_link_libraries(${EXE} tsutil)

if(NOT ${CMAKE_VERSION} VERSION_LESS 3.8.0)
    get_target_property(EXE_SOURCES ${EXE} SOURCES)
    source_group(TREE ${CMAKE_CURRENT_LIST_DIR}/src
        FILES ${EXE_SOURCES}
    )
endif()
