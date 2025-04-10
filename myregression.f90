module util
contains        
        subroutine load_example(filename, dataset)
                implicit none
                character(len=100), intent(in) :: filename
                real (kind = 4), allocatable, dimension(:,:), intent(inout):: dataset
                logical :: file_exists
                integer :: io, iostat
                real (kind = 4), allocatable, dimension(:,:) :: tmp_array
                character(len=100) :: iomsg
                integer :: i, j, list_size
                integer :: posit

                character(len=100) :: header
                integer :: m
                
                inquire(file=filename, exist=file_exists)
                if (.not.file_exists) then
                        print *, "could not locate file "//filename
                        return
                end if

                open(newunit=io, file=filename)

                iostat=0
                read(io, '(A)', iostat=iostat, iomsg=iomsg) header

                posit = 1
                m=0
                do while (posit.gt.0)
                        posit = index(header, ",")
                        m = m + 1
                        header = header(posit+1:)
                end do
                

                if (iostat.ne.0) then
                        print *, "could not read file "// filename 
                        return 
                end if

                i = 1
                list_size = 100
                allocate(dataset(m, list_size))
                do
                        if (i.ge.list_size) then
                               call move_alloc(dataset, tmp_array)

                               list_size = list_size + 100
                               allocate(dataset(m, list_size))
                               
                               dataset(:,:i) = tmp_array
                        end if
                        read(io, *, IOSTAT=iostat, IOMSG=iomsg) (dataset(j, i), j=1, m)
                        
                        if (iostat.eq.0) then
                                i = i + 1 
                        else if (iostat.eq.-1) then 
                                ! i = number of lines in file
                                i = i-1
                                exit
                        else
                                print *, "Error could not read the file: "//iomsg
                                stop
                        end if
                end do
                close(io)
               
          
                call move_alloc(dataset, tmp_array)
                allocate(dataset(m, i))
                dataset(:,:i) = tmp_array(:,:i)
        end subroutine
        
        subroutine scale_features(dataset, m, n)
                implicit none
                real (kind = 4), allocatable, dimension(:,:), intent(inout) :: dataset
                integer (kind = 4), intent(in) :: m
                integer (kind = 4), intent(in) :: n
                real (kind = 4) :: mean
                real (kind = 4) :: stddev
                integer (kind = 4) :: column_number
                
                do column_number = 1, m
                        mean = sum(dataset(column_number,:)) / n 
                        stddev = sqrt(sum((dataset(column_number,:) - mean)**2)/n)
                        dataset(column_number,:) = (dataset(column_number,:) - mean) / stddev
                end do
        end subroutine
end module


program main
        use util
        implicit none
        real (kind = 4), allocatable, dimension(:,:) :: dataset
!        integer (kind = 4) m
!        integer (kind = 4) n
!        real (kind = 4), allocatable, dimension(:,:) :: a
!        real (kind = 4), allocatable, dimension(:) :: b 
!        real (kind = 4), allocatable, dimension(:) :: x
        integer (kind = 4):: m = 3
        integer (kind = 4):: n = 464
        real (kind = 4):: a(n, m)
        real (kind = 4):: b(n)
        real (kind = 4):: x(m)
        character(len=50) :: progname
        character(len=100) :: filename
        integer (kind = 4) :: i, j
        

        call get_command_argument(0, progname)
        if (command_argument_count().ne.1) then
                print *, "Usage: "// trim(progname)//" <filename>"
               stop
        else
               call get_command_argument(1, filename) 
        end if        

        call load_example(filename, dataset)
        
!        m = size(dataset, dim=1) 
!        n = size(dataset, dim=2)
       
        print *, n

        a = dataset(1:m-1,:)
        call scale_features(a, m, n)

        b = dataset(m,:)
        
        call normal_l2 ( a, n, m-1, b, 0, x )
        !write (*, *)  ((a(i, j), " ", i=1, size(a, dim=1)), j, new_line("A"), j=1, size(a, dim=2))



end program
