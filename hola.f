      program Este es mi primer programa
c     este es un programa que imprime hola
      implicit none  !esta int desactiva la declaracion implicita
      real*8 x,y,z(8),sum,prod,div
      open(10,file="juanito.txt")
c      write(*,*) 'HOLA'
      write(10,*) 'Hola en el archivo'
      x=2.1342
      y=3.118
      sum=x+y
      prod=x*y
      div=x/y
      write(*,*) sum
      end program