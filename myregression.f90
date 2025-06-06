program main
        use util
        implicit none
        double precision, allocatable, dimension(:,:) :: dataset
        integer (kind = 4) m
        integer (kind = 4) n
        double precision, allocatable, dimension(:,:) :: X 
        double precision, allocatable, dimension(:) :: y 
        double precision, allocatable, dimension(:) :: model
        double precision, allocatable, dimension(:,:) :: Xinv
        character(len=50) :: progname
        character(len=100) :: filename
        
        integer (kind = 4) :: info
        integer (kind = 4) :: ierror
        double precision, allocatable, dimension(:) :: pivot
        
        double precision:: flarea
        integer :: bdrms
        integer :: bthrms
        double precision :: estimate
        ! load the csv
        call get_command_argument(0, progname)
        if (command_argument_count().ne.1) then
                print *, "Usage: "// trim(progname)//" <filename>"
               stop
        else
               call get_command_argument(1, filename) 
        end if        

        call read_csv(filename, dataset)
        
        !get the size of the dataset
        m = size(dataset, dim=1) 
        n = size(dataset, dim=2)
       
        allocate(X(m, n))
        !fill the first column with 1s 
        X(1, :) = 1
        !separate features from target vales
        X(2:m, :) = dataset(1:m-1,:)
        y = dataset(m,:)
        
        
        !inverse(transpose(X).X)
        X = transpose(X)
        Xinv = matmul(transpose(X), X)

        call inverse(Xinv)

        !transpose(X).y
        model = matmul(transpose(X), y)

        allocate(pivot(size(Xinv, dim=1)))
 
        !inverse(transpose(X).X).model = transpose(X).y
        call DGESV(size(Xinv, dim=1), 1, Xinv, size(Xinv, dim=1), pivot, model, size(model), info) 

       
        !use the model to make a prediction

        print *, "Total floor area (meters squared)"
        ierror = 1
        do while (ierror.ne.0)
                read(*, '(d20.0)', iostat=ierror) flarea
                if (ierror.ne.0) then
                        print *, "Please enter a valid decimal"
                end if
        end do
        
        print *, "Number of bedrooms"
        ierror = 1
        do while (ierror.ne.0)
                read(*, '(i10)', iostat=ierror) bdrms
                if (ierror.ne.0) then
                        print *, "Please enter a valid integer"
                end if
        end do

        print *, "Number of bathrooms"
        ierror = 1
        do while (ierror.ne.0)
                read(*, '(i10)', iostat=ierror) bthrms
                if (ierror.ne.0) then
                        print *, "Please enter a valid integer"
                end if
        end do
        
        estimate = model(1) + model(2) * flarea + model(3) * bdrms + model(4) * bthrms
        print *, "Your house should cost (thousands):"
        print *, estimate
end program
