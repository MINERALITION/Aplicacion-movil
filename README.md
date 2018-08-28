# supreme-rotary-phone
App mobile.
A=Plot[funcion[x],{x,intervalo[[1]],intervalo[[2]]}];

longint=intervalo[[2]]-intervalo[[1]];

propo=longint/(individuos+1);

lista={intervalo[[1]],intervalo[[2]]};

For[i=1,i<=individuos,i++,

lista=Append[lista,N[intervalo[[1]]+i*propo]]

];

Print[“inicio: “,Sort[lista]];

(*Iteraciones*)

For[kk=1,kk<=iter,kk++,

{

Print[“*******
Iteracion: “, kk];

funlis=N[funcion[lista]];

(*Reproduccion*)

While[Length[lista]<=poblacion,

{

alfa=Random[Real,{Min[funlis], Max[funlis]}];

Print[“Se
reproducen los menores de: “,alfa];

longlist=Length[lista];

For[i=1,i<=longlist,i++,

{

If[funlis[[i]]<=alfa,

{

beta=Random[Integer,{1,longlist}];

gama=Random[Real,{0,1}];

res=N[gama*lista[[i]]+(1-gama)*lista[[beta]]];(*nuevo
individuo*)

lista=Append[lista,res];

funlis=Append[funlis,funcion[res]]

}

]

}

]

}

];

Print[“Hijos: “,Sort[lista]];

(*
Mutaciones *)

If[Mod[kk,nuitermuta]==0,

{

alfa=Random[Integer,{1,numuta+1}];

For[s=1, s<=alfa, s++,

{

beta=Random[Real,{intervalo[[1]],intervalo[[2]]}];

Print[“Nueva Mutacion: “,beta, ” con valor: “,funcion[beta]];

lista=Append[lista,beta];

funlis=Append[funlis,funcion[beta]]

}

]

}

];

(*Muertes*)

qui=Length[lista]-poblacion-1;

Print[“Quitar
“,qui,” individuos.”];

salto=0;

While[qui>=0,

{

If[salto==0, alfa=Random[Real, {Min[funlis], Max[funlis]}]]

Print[“Se mueren el
inmediatamente mayores de: “,alfa];

mi=10^100; (* Un numero
muy grande *)

funlis=N[funcion[lista]];

For[j=1,j<=Length[lista],j++,

{

If[funlis[[j]]>alfa
&& funlis[[j]]<=mi,

{

mi=funlis[[j]];

jj=j

}

]

}

];

If[mi<10^100, (* el
numero muy grande de arriba *)

{

qui–;

lista=Delete[lista,jj];

funlis=Delete[funlis,jj];

salto=0

},

{

alfatem=alfa;

alfa=alfatem/2;

salto=1

}

]

}

];

Print[“Supervivientes:
“,Sort[lista]];

B=ListPlot[Table[{lista[[i]], funlis[[i]]}, {i,
Length[lista]}],

PlotStyle->{PointSize[0.03],RGBColor[1, 0,
0]}];

Show[A,B];

}

];

(*Solucion*)

res=funcion[lista];

Print[“Resultado: minimo:”,Min[res],” en el punto:
“,lista[[Position[res,Min[res]][[1]][[1]]]]]
