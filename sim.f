      program simpson
       implicit none
      real suma1,suma2,total,b,a,h,f
      integer i,n	  
	     
	     write(*,*) 'ingrese a,b'
	     read(*,*) a,b
	     write(*,*) 'ingrese n'
	     read(*,*) n
		 h=(b-a)/n
		 
		 suma1=0
	     suma2=0
	     total=0
		
	     do i=2,n-1,2
		 suma1=suma1+f(a+h*i)
		    end do
		 do i=1,n,2
		 suma2=suma2+f(a+h*i)
		    end do
		total=(h/3)*(f(a)+2*suma1+4*suma2+f(b))	
			write(*,*) 'el resultado es',total
			
	     end program
		 
		 
		    function f(x)
            implicit none
            real*8 f,x
            f=sin(x)
            return
             end function			