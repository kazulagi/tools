module UserClass
    
    implicit none
    
    type:: User_
        integer,private :: hand
    contains
        procedure,public :: Init    => Init_User
        procedure,public :: setHand => setHand_User 
        procedure,public :: showHand => showHand_User
    end type

contains

subroutine Init_User(obj,InNum)
    class(User_),intent(inout)::obj
    integer,optional,intent(in)::InNum

    ! constructor
    obj%hand = 0
end subroutine


subroutine setHand_User(obj)
    class(User_),intent(inout)::obj

    print *, "1 : Paper, 2 : Stone, 3 : Scissors"
    print *, "Input 1, 2 or 3"
    read(*,*) obj%hand
end subroutine



subroutine showHand_User(obj)
    class(User_),intent(inout)::obj
    
    if(obj%hand==1)then
        print *, "Paper"
    elseif(obj%hand==2)then
        print *, "Stone"
    elseif(obj%hand==3)then
        print *, "Scissors"
    else
        print *, "No inputs"
    endif
end subroutine

end module

program main
    use UserClass
    implicit none

    type(User_) :: Fox
    

    call Fox%Init()

    call Fox%setHand()
    call Fox%showHand()


end program 