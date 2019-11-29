      program regla trapezoidal
c     este es un programa que calcula
c     la integral por la regla del trapecio
      implicit none  !esta int desactiva la declaracion implicita
      real*8 x0,xn,h,x1,sum,inte
      integer n,i
c      open(10,file="juanito.txt")
      write(*,*) 'ingrese x0'
      read(*,*) x0
      write(*,*) 'ingrese xn'
      read(*,*) xn
      write(*,*) 'ingrese cuantos intervalos '
      read(*,*) n
      h=(xn-x0)/n
      sum=0.0
      do i=1,n
        x1=x0+h
        inte=h/2*(sin(x0)+sin(x1))
        sum=inte+sum   !acumula la integral
        x0=x1
      end do
     
      
      write(*,*) sum
      end program