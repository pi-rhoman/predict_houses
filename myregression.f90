program main
        implicit none
        real (kind = 4), allocatable, dimension(:,:) :: a
        real (kind = 4), allocatable, dimension(:) :: b
        integer (kind = 4) m
        integer (kind = 4) n
        real (kind = 4), allocatable, dimension(:) :: x

        m = 2
        n = 3
        
        allocate ( a(m,n) )
        allocate ( b(n) )
        allocate ( x(m) )
        
        a = reshape ((/ 1,2,3,4,5,6 /), (/2,3/))
        b = (/7,8,9/)


        call normal_l2 ( a, m, n, b, 0, x )

        print *, x
        call timestamp ()

end program
