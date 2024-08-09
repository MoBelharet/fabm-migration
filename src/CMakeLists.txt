add_library(fabm_models_migration OBJECT
	    migration_model_library.F90
	    migration_vertical_distribution.F90
	    migration_weight_distribution.F90
	    migration_move.F90
	    )

target_include_directories(fabm_models_migration PRIVATE ${CMAKE_CURRENT_SOURCE_DIR})
add_dependencies(fabm_models_migration fabm_base)


    #add_dependencies(fabm_models_migration fabm_base)
