PROGRAM average_array_section

  IMPLICIT NONE
  INTEGER, PARAMETER :: nx = 100, ny = 100, nits = 1000000
  REAL, DIMENSION(0:nx+1, 0:ny+1) :: u, uprime
  INTEGER :: ix, iy, iit

  u = 0.0

  DO iit = 1, nits
    !Apply boundary conditions
    u(:,0) = 1.0
    u(:,ny+1) = 10.0
    u(0,:) = 1.0
    u(nx+1,:) = 10.0
    uprime(1:nx,1:ny) = 0.25 * (u(0:nx-1,1:ny) + u(2:nx+1,1:ny) &
        + u(1:nx, 0:ny-1) + u(1:nx, 2:ny+1))
    u = uprime
  END DO

  WRITE(20,*) u(1:nx,1:ny)


END PROGRAM average_array_section
