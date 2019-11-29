       program trapecio
c Se calcula el polinomio por el metodo del trapecio
         implicit none
		    real a,b,total,f,f1,error
            integer i
c Valores iniciales       
		 a=0
		 b=0
		 total=0
         write(*,*) 'ingrese valor inicial,final'
         read (*,*) a,b		      
	     total=((b-a)/(2.0*n))*(f(a)+f(b))

			write(*,*) 'El resultado de nuestra integracion es',total
		    End program
			
		    Function f(x)
			   implicit none
			   real f,x
			   f=-0.13161500+(5.5345178*x)-(3.0606735*x**2)+(1.1624274*x**3)
     .-(0.0877127424*x**4)		 
			
			   return
     
			   end function