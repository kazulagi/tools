module MathClass

    type :: Math_
        real(8),allocatable :: Array(:,:)
        real(8),allocatable :: vector(:)
    contains
        procedure :: Init => InitializeArray
        procedure :: setRotationMat => setRotationMat
    end type

contains

subroutine InitializeArray(obj,dim)
    class(Math_),intent(inout)::obj
    integer,intent(in) :: dim

    if(allocated(obj%Array) )then
        deallocate(obj%Array)
    endif

    if(allocated(obj%Vector) )then
        deallocate(obj%Vector)
    endif

    

    allocate(obj%Array(dim,dim), obj%Vector(dim) )

    obj%Array(:,:)=0.0d0
    obj%Vector(:) =0.0d0
    
end subroutine


subroutine setRotationMat(obj,angle)
    class(Math_),intent(inout)::obj
    real(8),intent(in) :: angle
    
    if(size(obj%Array,1) /= 2 )then
        print *, "Error :: (size(obj%Array,1) /= 2 )"
        return
    else
        obj%Array(1,1)=cos(angle)   ;obj%Array(1,2) =-sin(angle)    
        obj%Array(2,1)=sin(angle)   ;obj%Array(2,2)= cos(angle)
    endif

end subroutine

end module 

program main
    use MathClass
    implicit none

    real(8) :: RotatedVector(2),x,y
    type(Math_) :: obj

    call obj%Init(2)
    ! Input
    open(10,file="vector2D.inp")
    read(10,*) x,y,obj%vector(:)
    close(10)

    print *, "Input rotation angle (rad.)"

    read(*,*) x
    call obj%setRotationMat(x)


    ! Output
    open(20,file="rotated2D.inp")
    write(20,*) "0.0 0.0",matmul(obj%Array,obj%Vector)
    close(20)

end program main