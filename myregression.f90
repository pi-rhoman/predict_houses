program main
        use util
        implicit none
        double precision, allocatable, dimension(:,:) :: dataset
        integer (kind = 4) m
        integer (kind = 4) n
        double precision, allocatable, dimension(:,:) :: a
        double precision, allocatable, dimension(:) :: b 
        double precision, allocatable, dimension(:) :: x
        double precision, allocatable, dimension(:) :: ATb
        double precision, allocatable, dimension(:,:) :: Ainv
        character(len=50) :: progname
        character(len=100) :: filename
        
        integer (kind = 4) :: info
        double precision, allocatable, dimension(:) :: pivot
        

        call get_command_argument(0, progname)
        if (command_argument_count().ne.1) then
                print *, "Usage: "// trim(progname)//" <filename>"
               stop
        else
               call get_command_argument(1, filename) 
        end if        

        call read_csv(filename, dataset)
        
        m = size(dataset, dim=1) 
        n = size(dataset, dim=2)
       

        allocate(a(m, n))
        a(1, :) = 1
        a(2:m, :) = dataset(1:m-1,:)
        !call scale_features(a, m, n)
        
        b = dataset(m,:)
        
        allocate(x(m))

        a = transpose(a)
        Ainv = matmul(transpose(a), a)
        print *, size(Ainv, dim=1), size(Ainv, dim=2)

        call inverse(Ainv)
        print *, size(Ainv, dim=1), size(Ainv, dim=2)

        print *, size(b)
        ATb = matmul(transpose(a), b)
        
        print *, size(ATb)

        allocate(pivot(size(Ainv, dim=1)))
 
        call DGESV(size(Ainv, dim=1), 1, Ainv, size(Ainv, dim=1), pivot, ATb, size(ATb), info) 

        print *, ATb

end program
