      program N_R
c     este es un programa que calcula
c     la raiz de una funcion
      implicit none  !esta int desactiva la declaracion implicita
      real*8 f0,Re, kd,tole,g,dg,fsol,g0,dg0
      integer iter
      write(*,*) 'ingrese f0'
      read(*,*) f0
      write(*,*) 'ingrese el num de Reynolds '
      read(*,*) Re
      write(*,*) 'ingrese el valor K/D '
      read(*,*) kd
      tole=1e-8
      g0=1.0
      iter=0
      do while (abs(g0)>tole.and.iter<=100) 
      g0=g(f0,Re,kd)
      dg0=dg(f0,Re,kd)
      fsol=f0-g0/dg0     
      f0=fsol
      g0=g(f0,Re,kd)
      iter=iter+1
      end do
      call imprimir(fsol,iter,g0)
           
      end program
      
      function g(f,Re,kd)
c     calcula g dado f Re kd
      real*8 f,Re,kd,g
      g=1.0/sqrt(f)+2.0*log(2.51/(Re*sqrt(f))+kd/3.71)
      return
      end
      
      function dg(f0,Re,kd)
      real*8 f0,Re,kd,dg
C      calcula derivada de g      
      dg=-.5*f0**(-1.5)-(1.0/(2.51/(Re*sqrt(f0))+kd/3.71))*
     . (2.51/Re*f0**(-1.5))
      return
      end
      
      subroutine imprimir(fsol,iter,g0)
      real*8 fsol,g0
      integer iter
      write(*,*) 'la solucion es : ' 
      write(*,*) fsol
      write(*,*) 'la cantidad de iteraciones fue :'
      write(*,*) iter
      write(*,*) 'el valor de g en fsol es :'
      write(*,*) g0
      return
      end
