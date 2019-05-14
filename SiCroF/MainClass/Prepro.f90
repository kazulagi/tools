program Preprocessing
    use TermClass
    use DictionaryClass
    use PreprocessClass

    implicit none
    
    type(Dictionary_)       :: InfileList
    type(MPI_)              :: MPIData
    type(PreProcessing_)    :: PreObj
    type(Term_)             :: term
    character * 200         :: name,name1,name2,name3,name4,ElemType,SolverName
    call MPIData%Start()
    call term%Init()
     
    ElemType = "LinearRectangularGp4"
    name1="debug/scandata/case1GM.png"
    name2="debug/scandata/case2GM.png"
    name3="debug/scandata/case3GM.png"
    name4="debug/scandata/case4GM.png"

    ! load file names
    call InfileList%Init(4) ! Constractor
    call InfileList%Input(1,name1)
    call InfileList%Input(2,name2)
    call InfileList%Input(3,name3)
    call InfileList%Input(4,name4)

    name = InfileList%Get(MPIData%MyRank+1)
     print *, "My_rank : ",MPIData%MyRank,"InfileName : ",trim(name)

    ! Get Pixcel
    call PreObj%Init(Default=.true.)  ! Constractor
    call PreObj%ImportPictureName(name)
    call PreObj%GetPixcelSize(MPIData)
    call PreObj%SetColor(28,255,255)
    call PreObj%GetPixcelByRGB(MPIData,err=10,onlycoord=.true.)

    ! Get Outline
    call PreObj%GetSurfaceNode(MPIData)
    call PreObj%AssembleSurfaceElement(MPIData,dim=2,threshold=10,DelRange=10)
    

    ! Reduce Number of Surface Nodes
    call PreObj%ReduceSize(MPIData,interval=100)


    ! Convert SurfaceNod to .geo
    call PreObj%ExportGeoFile(MPIData)



    ! Run Gmsh to convert .geo to .msh
    call PreObj%ConvertGeo2Msh(MPIData)
    call PreObj%ConvertGeo2Inp(MPIData)
    call PreObj%ConvertGeo2Mesh(MPIData)
    
   

    ! Convert .msh to .scf
    !call PreObj%ConvertMsh2Scf(MPIData,ElementType=ElemType)
    call PreObj%ConvertMesh2Scf(MPIData,ElementType=ElemType)
    call PreObj%Convert3Dto2D()
    call PreObj%SetScale(scalex=20.0d0,scaley=10.0d0)
    
    SolverName="FiniteDeform_"
    call PreObj%SetSolver(InSolverType=SolverName)
    call PreObj%SetUp(NoFacetMode=.true.)
    
    ! Settup Boundary Condition
    call PreObj%SetSizeOfBC(Dirichlet=.true. , NumOfValue=4)
    call PreObj%SetBC(Dirichlet=.true., xmax=40.0d0, val=20.0d0,val_id=1)
    call PreObj%SetBC(Dirichlet=.true., xmin=50.0d0, val=10.0d0,val_id=2)
    call PreObj%SetBC(Dirichlet=.true., ymax=2.0d0,  val=12.0d0,val_id=3)
    call PreObj%SetBC(Dirichlet=.true., ymin=9.0d0,  val=90.0d0,val_id=4)
    
    call PreObj%SetSizeOfBC(Neumann=.true. , NumOfValue=4)
    call PreObj%SetBC(Neumann=.true., xmax=40.0d0, val=20.0d0,val_id=1)
    call PreObj%SetBC(Neumann=.true., xmin=50.0d0, val=10.0d0,val_id=2)
    call PreObj%SetBC(Neumann=.true., ymax=2.0d0,  val=12.0d0,val_id=3)
    call PreObj%SetBC(Neumann=.true., ymin=9.0d0,  val=90.0d0,val_id=4)
    
    call PreObj%SetSizeOfBC(Initial=.true. , NumOfValue=4)
    call PreObj%SetBC(Initial=.true., xmax=40.0d0, val=20.0d0,val_id=1)
    call PreObj%SetBC(Initial=.true., xmin=50.0d0, val=10.0d0,val_id=2)
    call PreObj%SetBC(Initial=.true., ymax=2.0d0,  val=12.0d0,val_id=3)
    call PreObj%SetBC(Initial=.true., ymin=9.0d0,  val=90.0d0,val_id=4)

    call PreObj%SetControlPara(OptionalTol=1.0d0,OptionalItrTol=100,OptionalTimestep=100,OptionalSimMode=1)
    
    ! Export Object
    call PreObj%Export(MPIData)

    call MPIData%End()

end program 