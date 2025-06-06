
module util
        use, intrinsic :: iso_fortran_env
contains        
        subroutine read_csv(filename, dataset)
                ! reads csv file containing real numbers into a 2D array provided by dataset
                ! filename - path to the csv file
                ! dataset - an empty array , allocatable, dimension(:,:)
                
                implicit none
                character(len=100), intent(in) :: filename
                double precision, allocatable, dimension(:,:), intent(inout):: dataset
                logical :: file_exists
                integer :: io, iostat
                double precision, allocatable, dimension(:,:) :: tmp_array
                character(len=100) :: iomsg
                integer :: i, j, list_size
                integer :: posit

                character(len=100) :: header
                integer :: m
                
                ! find the file
                inquire(file=filename, exist=file_exists)
                if (.not.file_exists) then
                        print *, "could not locate file "//filename
                        return
                end if

                open(newunit=io, file=filename)

                ! read the header
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

                ! read the file
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
               
          
                ! resize the array for memory efficiency
                call move_alloc(dataset, tmp_array)
                allocate(dataset(m, i))
                dataset(:,:i) = tmp_array(:,:i)
        end subroutine
        
        subroutine scale_features(dataset, m, n)
                ! devide all columns in a 2D array by the standard deviation and subtract the mean
                ! dataset - the dataset to scale
                ! m - the number of features(columns)
                ! n - the number of examples(rows)
                implicit none
                double precision, allocatable, dimension(:,:), intent(inout) :: dataset
                integer (kind = 4), intent(in) :: m
                integer (kind = 4), intent(in) :: n
                double precision :: mean
                double precision :: stddev
                integer (kind = 4) :: column_number
                
                do column_number = 2, m
                        mean = sum(dataset(column_number,:)) / n 
                        stddev = sqrt(sum((dataset(column_number,:) - mean)**2)/n)
                        dataset(column_number,:) = (dataset(column_number,:) - mean) / stddev

                end do
        end subroutine

        subroutine inverse( A )
                ! find the inverse of a real matrix A
                ! A - the matrix to invert

                implicit none
                double precision, dimension(:, :), intent(in) :: A
                double precision, dimension(size(A, dim=1), size(A, dim=2)):: Ainv
                double precision, dimension(size(A, dim=1)) :: work
                integer, dimension(size(A, dim=1)) :: pivots
                integer :: n, info 

                Ainv = A
                n = size(A, dim= 1)
                ! compute the LU factorization of A
                call DGETRF(n, n, Ainv, n, pivots, info)
                if (info.ne.0) stop 'Cannot invert numerically singular matrix'
                call DGETRI(n, Ainv, n, pivots, work, n, info) 
                if (info.ne.0) stop 'Cannot invert matrix'
      end subroutine


    
end module
