PROGRAM average_loops

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
    DO iy = 1, ny
      DO ix = 1, nx
        uprime(ix,iy) = 0.25 * (u(ix-1,iy) + u(ix+1,iy) &
            + u(ix,iy-1) + u(ix,iy+1))
      END DO
    END DO
    u = uprime
  END DO

  WRITE(10,*) u(1:nx,1:ny)


END PROGRAM average_loops
