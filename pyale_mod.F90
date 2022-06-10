module pyale_mod
  use MOM_domains, only : MOM_domains_init, MOM_domain_type
  use MOM_file_parser, only : param_file_type, open_param_file

  implicit none ; private

  public :: create_domain
contains

  subroutine create_domain
    type(param_file_type) :: param_file
    type(MOM_domain_type), pointer :: domain => NULL()

    call open_param_file("MOM_input", param_file)
    call MOM_domains_init(domain, param_file, symmetric=.true., domain_name="MOM_in")
  end subroutine create_domain
end module pyale_mod
