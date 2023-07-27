function [teuler,yeuler,ev]=fill_prob2(f,intv,y0,N,TOL,nmax) % Se implementa la función correspondiente a 
                                                                  % la iteración simple+dato inicial
                                                                  % el valor del paso anterior,
    yeuler = y0;                                                 % Se toman los valores iniciales que se dan como argumento
    a = min(intv);                                               
    b = max(intv); 
    h = (a + b)/N;
    teuler = a;
    
    tk = a;
    yk = y0;
    
    ev = 0;                                                       % el número de evaluaciones al inicio es cero, pues no ha habido ninguna iteración
    
    faux = @(t, y, f, h, z) [y + h*f(t, z)];                      % se define la función auxiliar para la iteración simple
    
    
    for i = 1:N
        
        t = tk + h;                                               % se toma los valores del tiempo correspondientes al paso 
        
        y = yk;
        diff = 100;                                              % se toma un numero arbitrariamente grande para inicializar el error de la iteración simple
        k = 0;                                                    % se inicializa el conteo para la iteración simple 
        
        while diff > TOL && k <= nmax                             % por una parte se condiciona al while con un valor de tolerancia para el error diff, error de una iteración (de la iteración simple), y, por otra parte, se toma un numero máximo de iteraciones porque puede perfectamente el error de iteración no ser menor que la tolerancia, provocando un bucle infinito
        
            k = k + 1;
            z = faux(t, yk, f, h, y);                              % hay que tener en cuenta que durante este bucle sólo se está considerando el tiempo t=tk +h con h valor de paso que corresponda, pero se mantiene fijo así como también se toma el valor inicial de y del paso anterior pero éste no está fijo 
            diff = max(max(abs(z - y)));                           % aquí se calcula el error de iteración, en este caso se utiliza la norma infinito
            y = z;                                                 % se da el nuevo valor para la siguiente iteración 
            
            ev = ev + 1;                                           % se hace el conteo del número de iteraciones 
        
        end

        yeuler = [yeuler, y];                                       % se actualizan los valores de 'y' y t para los vectores respuesta de la función
        teuler = [teuler, t];  
        
        tk = t;                                                     % se actualiza los valores de t y 'y' para la siguiente iteración del  ciclo for
        yk = y;
        
    end
    

end

function [teuler,yeuler,ev]=mieulerimpfixpc(f,intv,y0,N,TOL,nmax)  % se implementa el tercer tipo de función dada por el enunciado 
    a = min(intv);                                                 % se preparan los datos de la mísma forma como se ha comentado en la anterior función
    b = max(intv);
    h = (a + b)/N;
    
    tk = a;
    yk = y0;
    
    teuler = a;
    yeuler = y0;
    
    ev = 0;
    
    faux = @(t, y, f, h, z) [y + h*f(t, z)];
    
    
    for i = 1:N                                         % la diferencia fundamental con respecto a la función anterior es que en este caso se va a utilizar para la iteración simple como valor inicial 'y' el calculado por el método de euler explícito 
        
        t = tk + h;
        
        y_pred = yk + h*f(tk, yk);           % aquí se calcula dicha 'y' que se va a utilizar en la iteración simple
        ev = ev + 1;
        
        y = y_pred;
        k = 0;
        diff = 100;
        
        while diff > TOL && k <= nmax           % se hace la iteración simple como en la anterior función pero teniendo en cuenta ahora que la 'y' de valor inicial es el calculado por el método de euler explícito anteriormente obtenido 
        
            k = k + 1;
            z = faux(t, yk, f, h, y);
            diff = max(max(abs(z - y)));
            y = z;
            
            ev = ev + 1;
        
        end

        yeuler = [yeuler, y];
        teuler = [teuler, t]; 
        
        tk = t;
        yk = y;
        
        
    end
end
function [tvals, yvals,ev,loopcount]=mieulerimpnwt(f,jfunc,intv,y0,N,TOL,nmax)  % se implementa el segundo tipo de función dada por el enunciado 


    
    a = min(intv);                                                                % se preparan los datos como antes se ha hecho
    b = max(intv);
    h = (a + b)/N;

    tk = a;
    yk = y0;
    
    tvals = a;                       % en este caso se cambia el nombre de los vectores respuesta
    yvals = y0;
    
    ev = 0;
    loopcount = 0;                   % esta variable no sirve
            
    faux = @(t, y, f, h, z) [y + h*f(t, z) - z];           % como se va a utilizar la iteración tipo Newton se requiere modifiaciones de la función auxiliar 
    jfuncaux = @(t, y, jfunc, h, z) [h*jfunc(t, z) - jfunc(t, z)\jfunc(t, z)]; % en este caso se requieren dos funciones auxiliares

    
    for i = 1:N
        
        t = tk + h;
        
        y = yk;    % el método toma como valor inicial para la iteración el valor anterior
        k = 0;
        diff = 100;
        
        while diff > TOL && k <= nmax
            
            k = k + 1;
            z = y - jfuncaux(t, yk, jfunc, h, y)\faux(t, yk, f, h, y); % es en este punto donde reside la mayor diferencia, aquí se implementa la iteración de tipo Newton
            diff = max(max(abs(z -y)));
            y = z;
            
            ev = ev + 2;       % se evalu dos veces las funciones 

        end
        
        yvals = [yvals, y];
        tvals = [tvals, t];
        tk = t;
        yk = y;
        
        
    end
    
end
function [tvals, yvals,ev]=mieulerimpnwtpc(f,jfunc,intv,y0,N,TOL,nmax) % se implementa el cuarto tipo de función  


   
    a = min(intv);                                                      % se toman los valores y funciones auxiliares como en la función anterior 
    b = max(intv);
    h = (a + b)/N;
    
    tk = a;
    yk = y0;
    
    tvals = a;
    yvals = y0;
    
    ev = 0;
    
    faux = @(t, y, f, h, z) [y + h*f(t, z) - z];
    jfuncaux = @(t, y, jfunc, h, z) [h*jfunc(t, z) - jfunc(t, z)\jfunc(t, z)];
    
    
    for i = 1:N
        
        t = tk + h;
        
        y_pred = yk + h*f(tk, yk); %predictor                              % fijarse que aquí se toma el valor inicial para la iteración Newton calculado a partir del método de Newton explícito; por el resto no hay ninguna diferencia más con respecto a la anterior función
        ev = ev + 1;
        
        y = y_pred;
        k = 0;
        diff = 100;
        
        while diff > TOL && k <= nmax
            
            k = k + 1;
            z = y - jfuncaux(t, yk, jfunc, h, y)\faux(t, yk, f, h, y);
            diff = max(max(abs(z -y)));
            y = z;
            
            ev = ev + 2;                                                  
            
        end
        
        yvals = [yvals, y];
        tvals = [tvals, t];
        tk = t;
        yk = y;                                % hasta aquí la función es absolutamente igual en código a la función anterior 
        
        
    end
    

end
function [ttrap,ytrap,ev,loopcount]=mitrapfix(f,intv,y0,N,TOL,nmax)  % esta función es similar a la primera función euler implícito



    a = min(intv);
    b = max(intv);
    h = (a + b)/N;
    
    tk = a;
    yk = y0;
    
    ttrap = a;
    ytrap = y0;
    
    ev = 0;
    loopcount = 0;        % a diferencia de la función correspondiente a euler implícito se añade una nueva variable que cuenta los loops que hace el bucle while
    
    faux = @(t, y, f, h, z) [y + (h/2)*(f(t - h, y) + f(t, z))];  % aquí reside la mayor diferencia con respecto a la función de euler correspondiente; pues se considera la función auxiliar correspondiente al método de trapecio
    
    
    for i = 1:N
        
        t = tk + h;
        
        y = yk;
        k = 0;
        diff = 100;
        
        while diff > TOL && k <= nmax
        
            k = k + 1;
            z = faux(t, yk, f, h, y);
            diff = max(max(abs(z - y)));
            y = z;
            
            ev = ev + 2;
            loopcount = loopcount + 1;
        
        end
        
        ttrap = [ttrap, t];
        ytrap = [ytrap, y];
        
        tk = t;
        yk = y;
        
    end
                                          %%el resto del código es
                                          %%virtualmente el mísmo comparandolo con la función de
                                          %%euler implícito correspondiente anteiormente comentada
                                       

end
function [ttrap,ytrap,ev,loopcount]=mitrapfixpc(f,intv,y0,N,TOL,nmax) % el código de esta función es virtualmente el mísmo que el correspondiente a la función mieulerimpfixpc salvo cambiando la función auxiliar 



    a = min(intv);
    b = max(intv);
    h = (a + b)/N;
    
    tk = a;
    yk = y0;
    
    ttrap = a;
    ytrap = y0;
    
    ev = 0;
    loopcount = 0;            % también se cuenta esta nueva variable antes comentada
    
    faux = @(t, y, f, h, z) [y + (h/2)*(f(t - h, y) + f(t, z))];  % de nuevo la diferencia fundamental con respecto a la función mieulerimpfixpc es considerar esta función auxiliar 
    
    
    for i = 1:N
        
        t = tk + h;
        
        y_pred = yk + h*f(tk, yk); %Predictor
        ev = ev + 1;
        
        y = y_pred;
        k = 0;
        diff = 100;
        
        while diff > TOL && k <= nmax
        
            k = k + 1;
            z = faux(t, yk, f, h, y);
            diff = max(max(abs(z - y)));
            y = z;
            
            ev = ev + 2;
            loopcount = loopcount + 1;
        
        end

        ttrap = [ttrap, t];
        ytrap = [ytrap, y];
        
        tk = t;
        yk = y;
        
    end
 
end
function [ttrap,ytrap,ev,loopcount]=mitrapnwt(f,jfunc,intv,y0,N,TOL,nmax)   % el código de esta función es virtualmente el mísmo que el correspondiente a la función mieulerimpnwt salvo cambiando la función auxiliar 


    a = min(intv);
    b = max(intv);
    h = (a + b)/N;
    
    tk = a;
    yk = y0;
    
    ttrap = a;
    ytrap = y0;
    
    ev = 0;
    loopcount = 0;
    
    faux = @(t, y, f, h, z) [y + (h/2)*(f(t - h, y) + f(t, z)) - z];
    jfuncaux = @(t, y, jfunc, h, z) [(h/2)*jfunc(t, z) - jfunc(t, z)\jfunc(t, z)];
    
    
    for i = 1:N
        
        t = tk + h;
        
        y = yk;
        k = 0;
        diff = 100;
        
        while diff > TOL && k <= nmax
            
            k = k + 1;
            z = y - jfuncaux(t, yk, jfunc, h, y)\faux(t, yk, f, h, y);
            diff = max(max(abs(z -y)));
            y = z;
            
            ev = ev + 4;
            loopcount = loopcount + 1;
            
        end
        
        ttrap = [ttrap, t];
        ytrap = [ytrap, y];
        
        tk = t;
        yk = y;
        
    end
 
end
function [ttrap,ytrap,ev,loopcount]=mitrapnwtpc(f,jfunc,intv,y0,N,TOL,nmax)   % el código de esta función es virtualmente el mísmo que el correspondiente a la funciónmieulerimpnwtpc salvo cambiando la función auxiliar 



    a = min(intv);
    b = max(intv);
    h = (a + b)/N;
    
    tk = a;
    yk = y0;
    
    ttrap = a;
    ytrap = y0;
    
    ev = 0;
    loopcount = 0;
    
    faux = @(t, y, f, h, z) [y + (h/2)*(f(t - h, y) + f(t, z)) - z];
    jfuncaux = @(t, y, jfunc, h, z) [(h/2)*jfunc(t, z) - jfunc(t, z)\jfunc(t, z)];
    
    
    for i = 1:N
        
        t = tk + h;
        
        y_pred = yk + h*f(tk, yk); %Predictor
        ev = ev + 1;
        
        y = y_pred;
        k = 0;
        diff = 100;
        
        while diff > TOL && k <= nmax
            
            k = k + 1;
            z = y - jfuncaux(t, yk, jfunc, h, y)\faux(t, yk, f, h, y);
            diff = max(max(abs(z -y)));
            y = z;
            
            ev = ev + 4;
            loopcount = loopcount + 1;
            
        end
        
        ttrap = [ttrap, t];
        ytrap = [ytrap, y];
        
        tk = t;
        yk = y;
        
    end
    
end

function [N_vect,Ev_vect,error_vect]=fcomparerrorimpnwt(met,func,jacfunc,intv,y0,N,yexact,M,TOL,nmax);


    
    N_vect = [];
    Ev_vect = [];
    error_vect = [];
    
    for i = 0:M
        
        Ni = (2^i)*N;
        
        if strcmp(met, 'eulerimpnw') == 1
        [t_met, y_met, Ev, loopcount] = mieulerimpnwt(func,jacfunc,intv,y0,Ni,TOL,nmax);
  
        elseif strcmp(met, 'eulerimpnwpc') == 1
        [t_met, y_met, Ev] = mieulerimpnwtpc(func,jacfunc,intv,y0,Ni,TOL,nmax);
        
        elseif strcmp(met, 'trapnw') == 1
        [t_met, y_met, Ev, loopcount] = mitrapnwt(func,jacfunc,intv,y0,Ni,TOL,nmax);
        
        elseif strcmp(met, 'trapnwpc') == 1
        [t_met, y_met, Ev, loopcount] = mitrapnwtpc(func,jacfunc,intv,y0,Ni,TOL,nmax);
        end
        
        y_exact = yexact(t_met);
        
        error = max(max(y_met - y_exact));
        
        N_vect = [N_vect, Ni];
        Ev_vect = [Ev_vect, Ev];
        error_vect = [error_vect, error];
        
    end
    
end 

function [N_vect,Ev_vect,error_vect]=fcomparerrorimpfix(met,func,intv,y0,N,yexact,M,TOL,nmax);



    N_vect = [];
    Ev_vect = [];
    error_vect = [];
    
    for i = 0:M
        
        Ni = (2^i)*N;
        
        if strcmp(met, 'eulerimpfix') == 1
        [t_met, y_met, Ev] = mieulerimpfix(func, intv,y0,Ni,TOL,nmax);
        
        elseif strcmp(met, 'eulerimpfixpc') == 1
        [t_met, y_met, Ev] = mieulerimpfixpc(func,intv,y0,Ni,TOL,nmax);
        
        elseif strcmp(met, 'trapfix') == 1
        [t_met, y_met, Ev,loopcount] = mitrapfix(func,intv,y0,Ni,TOL,nmax);
        
        elseif strcmp(met, 'trapfixpc') == 1
        [t_met,y_met,Ev,loopcount] = mitrapfixpc(func,intv,y0,Ni,TOL,nmax);
        end
        
        y_exact = yexact(t_met);
        
        error = max(max(y_met - y_exact));
        
        N_vect = [N_vect, Ni];
        Ev_vect = [Ev_vect, Ev];
        error_vect = [error_vect, error];
        
    end

end

function [p,q]=fcalcorden(N_vect,error_vect) 

end
