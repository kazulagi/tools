program main
    use LinearSolverClass
    implicit none

    real(8),allocatable::Amat(:,:),xvec(:),bvec(:)
    integer :: i,n

    open(10,file="test.inp")
    read(10,*) n
    allocate(Amat(n,n),xvec(n),bvec(n) )
    xvec(:)=0.0d0
    do i=1,n
        read(10,*) Amat(i,:)
    enddo
    do i=1,n
        read(10,*) bvec(i)
    enddo
    close(10)

    call gauss_jordan_pv(Amat, xvec, bvec, n)

    open(20,file="solution.txt")
    do i=1,n
        write(20,*) "x",i,"=",xvec(i)
    enddo
    close(20)


end program 