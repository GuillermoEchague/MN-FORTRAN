      program N_R
c     Escribe un comentario
      implicit none  !esta int desactiva la declaracion implicita
      real*8 f0,Re, kd,tole,g,dg,fsol,g0,dg0 !variables real
      integer iter !variable entera
	  open (file='datos1.dat',unit=5) !abre nombre, unidad
      write(*,*) 'ingrese f0' !Escribe el valor a ingresar
      read(*,*) f0 !asigna la variable
      
      tole=1e-8 !limpio valores iniciales
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
      call imprimir(fsol,iter,g0) !llama subrutina
           
      end program
      

      
      function dg(f0,Re,kd)!funcion g(variables)
      real*8 f0,Re,kd,dg !definicion variables 
      dg=!se define funcion dg
      return !no olvidar
      end
      
      subroutine imprimir(fsol,iter,g0) !subrutina(parametros)
      real*8 fsol,g0 !defino parametros
      write(*,*) 'la solucion es : ' 
      write(*,*) fsol
      return !no olvidar return
      end