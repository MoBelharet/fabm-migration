module migration_model_library

   use fabm_types, only: type_base_model_factory,type_base_model

   use migration_vertical_distribution
   use migration_weight_distribution
   use migration_move

   implicit none

   private

   type,extends(type_base_model_factory) :: type_factory
      contains
      procedure :: create
   end type

   type (type_factory),save,target,public :: migration_model_factory

contains

   subroutine create(self,name,model)
      class (type_factory),intent(in) :: self
      character(*),        intent(in) :: name
      class (type_base_model),pointer :: model

      select case (name)
         case ('vertical_distribution'); allocate(type_migration_vertical_distribution::model)
         case ('weight_distribution'); allocate(type_migration_weight_distribution::model)
         case ('move');      allocate(type_migration_move::model)

         ! Add new models here
         case default
            call self%type_base_model_factory%create(name,model)
      end select
   end subroutine create

end module
