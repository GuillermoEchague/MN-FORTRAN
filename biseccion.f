      program biseccion
	     implicit none
         integer i,n,j
	     real a,b,e,xi,xr,xu,test,g
		    write(*,*) 'ingrese intervalos'
		    read (*,*) a,b
		    write(*,*) 'ingrese numero de iteraciones'
		    read(*,*) n
			 xr=a
			 xu=b
		   do while (iter<=10) 
      g0=g(xi,xu)
      iter=iter+1
		    test=f(xi)*g(xr)
			   if (test < 0) then
               	xu=xr
                else if (test>0) then
                xi=xr
               end do
              			   
		  
			read(*,*) xr
		    end program
	  
	     Function f(x)
		    implicit none
	        real x,f
		    f=x**2+2*x+6
			   return
		     end Function
			 
		    Function g(xr,xu)
			   implicit none
               real g,xi,xu		 
			   g=(xi+xu)/2
			   return
			   end function
			   
			   Function test(xi,xr)
		       implicit none
			   real test,xi,xr
			   test=f(xi)*f(xr)
			   return
			   end function