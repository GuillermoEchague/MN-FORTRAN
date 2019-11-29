       program trapecio compuesto
c Se calcula el polinomio por el metodo del trapecio
         implicit none
		    real a,b,total,f,h,suma1
            integer i,n
          suma1=0
         write(*,*) 'ingrese valor inicial,final'
         read (*,*) a,b
		 write (*,*) 'ingrese un n impar'
		 read (*,*) n
            h=(b-a)/n		 
            do i=1,n-1
			   suma1=suma1+f(a+h*i)
			   end do	
            total=(h/2)*(f(a)+f(b)+2*suma1)
			write(*,*) 'El resultado de nuestra integracion es',total
		    End program
			
		    Function f(x)
			   implicit none
			   real f,x
			   f=-0.13161500+(5.5345178*x)-(3.0606735*x**2)+(1.1624274*x**3)
     .-(0.0877127424*x**4)	!5 espacios se agrega punto	 
			   return
			   end function