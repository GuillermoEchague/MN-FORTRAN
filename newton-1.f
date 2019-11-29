      program N_R
c     este es un programa que calcula
c     la raiz de una funcion
      implicit none  !esta int desactiva la declaracion implicita
      real*8 f0,Re, kd,tole,g,dg,fsol
      integer iter
      write(*,*) 'ingrese f0'
      read(*,*) f0
      write(*,*) 'ingrese el num de Reynolds '
      read(*,*) Re
      write(*,*) 'ingrese el valor K/D '
      read(*,*) kd
      tole=1e-8
      g=1.0
      iter=0
      do while (abs(g)>tole.and.iter<=100) 
      g=1.0/sqrt(f0)+2.0*log(2.51/(Re*sqrt(f0))+kd/3.71)
      dg=-.5*f0**(-1.5)-(1.0/(2.51/(Re*sqrt(f0))+kd/3.71))*
     . (2.51/Re*f0**(-1.5))
      fsol=f0-g/dg     
      f0=fsol
      g=1.0/sqrt(f0)+2.0*log(2.51/(Re*sqrt(f0))+kd/3.71)
      iter=iter+1
      end do
     
      write(*,*) ' la solucion es f0 '
      write(*,*) f0
      write(*,*) g
      write(*,*) iter
      end program
