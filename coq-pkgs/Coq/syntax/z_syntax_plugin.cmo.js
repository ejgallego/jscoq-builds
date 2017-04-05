(function(aC){"use strict";var
ag="Z",af="N",ae="positive",h=aC.jsoo_runtime,b=h.caml_new_string,ad=h.caml_register_global,C=h.caml_wrap_exception;function
c(a,b){return a.length==1?a(b):h.caml_call_gen(a,[b])}function
d(a,b,c){return a.length==2?a(b,c):h.caml_call_gen(a,[b,c])}function
ac(a,b,c,d){return a.length==3?a(b,c,d):h.caml_call_gen(a,[b,c,d])}function
D(a,b,c,d,e){return a.length==4?a(b,c,d,e):h.caml_call_gen(a,[b,c,d,e])}var
f=h.caml_get_global_data(),E=b("z_syntax_plugin"),e=[0,b("Coq"),[0,b("Numbers"),[0,b("BinNums"),0]]],g=f.Globnames,a=f.Bigint,K=f.Pp,L=f.CErrors,l=f.Names,i=f.Loc,B=f.Notation,ai=f.Libnames,ah=f.Util;c(f.Mltop[12],E);var
j=[248,b("Z_syntax.Non_closed_number"),h.caml_fresh_oo_id(0)],au=b('No negative numbers in type "N".'),av=[0,b("interp_N")],an=b('Only strictly positive numbers in type "positive".'),ao=[0,b("interp_positive")],aj=b(ae),ak=b(ae),ap=b("positive_scope"),aq=b(af),at=b(af),aw=b("N_scope"),ax=b(ag),ay=b(ag),aB=b("Z_scope");function
m(a){var
b=d(ah[17][14],l[1][6],a);return c(l[5][4],b)}function
p(b,a){var
e=c(l[1][6],a),f=m(b);return d(ai[17],f,e)}var
F=p(e,aj);function
q(b,a){return d(g[25],b,a)}var
al=c(l[1][6],ak),n=q(m(e),al),G=[0,[0,n,0],1],H=[0,[0,n,0],2],I=[0,[0,n,0],3],r=[3,G],s=[3,H],t=[3,I],am=[2,[0,n,0]];function
u(b,f){var
h=[0,[0,b,r,0]],i=[0,[0,b,t,0]],j=[0,[0,b,s,0]];function
e(k){var
g=c(a[8],k),f=g[1];return 0===g[2]?[4,b,j,[0,e(f),0]]:d(a[17],f,a[5])?i:[4,b,h,[0,e(f),0]]}return e(f)}function
J(a){var
b=c(K[3],an);return ac(L[6],[0,a],ao,b)}function
M(d,b){return c(a[18],b)?u(d,b):J(d)}function
k(b){switch(b[0]){case
0:if(d(g[5],b[1][2],t))return a[6];break;case
4:var
f=b[2];if(0===f[0]){var
e=b[3];if(e)if(!e[2]){var
h=e[1],i=f[1][2];if(d(g[5],i,s)){var
l=k(h);return c(a[11],l)}if(d(g[5],i,r)){var
m=k(h),n=c(a[11],m);return c(a[9],n)}}}break}throw j}function
N(a){try{var
b=[0,k(a)];return b}catch(a){a=C(a);if(a===j)return 0;throw a}}D(B[13],ap,[0,F,e],M,[0,[0,[0,[0,i[4],r,0]],[0,[0,[0,i[4],s,0]],[0,[0,[0,i[4],t,0]],0]]],N,1]);var
ar=c(l[1][6],aq),v=q(m(e),ar),O=[0,[0,v,0],1],P=[0,[0,v,0],2],w=[3,O],x=[3,P],Q=p(e,at),as=[2,[0,v,0]];function
R(b,e,c){return d(a[17],c,a[5])?[0,[0,b,w,0]]:[4,b,[0,[0,b,x,0]],[0,u(b,c),0]]}function
S(a){var
b=c(K[3],au);return ac(L[6],[0,a],av,b)}function
T(d,b){return c(a[20],b)?R(d,1,b):S(d)}function
U(b){switch(b[0]){case
0:if(d(g[5],b[1][2],w))return a[5];break;case
4:var
e=b[2];if(0===e[0]){var
c=b[3];if(c)if(!c[2]){var
f=c[1];if(d(g[5],e[1][2],x))return k(f)}}break}throw j}function
V(a){try{var
b=[0,U(a)];return b}catch(a){a=C(a);if(a===j)return 0;throw a}}D(B[13],aw,[0,Q,e],T,[0,[0,[0,[0,i[4],w,0]],[0,[0,[0,i[4],x,0]],0]],V,1]);var
W=p(e,ax),az=c(l[1][6],ay),o=q(m(e),az),X=[0,[0,o,0],1],Y=[0,[0,o,0],2],Z=[0,[0,o,0],3],y=[3,X],z=[3,Y],A=[3,Z],aA=[2,[0,o,0]];function
_(e,b){if(d(a[17],b,a[5]))return[0,[0,e,y,0]];if(c(a[20],b))var
g=z,f=b;else
var
g=A,f=c(a[22],b);return[4,e,[0,[0,e,g,0]],[0,u(e,f),0]]}function
$(b){switch(b[0]){case
0:if(d(g[5],b[1][2],y))return a[5];break;case
4:var
f=b[2];if(0===f[0]){var
e=b[3];if(e)if(!e[2]){var
h=e[1],i=f[1][2];if(d(g[5],i,z))return k(h);if(d(g[5],i,A)){var
l=k(h);return c(a[22],l)}}}break}throw j}function
aa(a){try{var
b=[0,$(a)];return b}catch(a){a=C(a);if(a===j)return 0;throw a}}D(B[13],aB,[0,W,e],_,[0,[0,[0,[0,i[4],y,0]],[0,[0,[0,i[4],z,0]],[0,[0,[0,i[4],A,0]],0]]],aa,1]);var
ab=[0,E,j,e,m,p,F,q,n,am,G,H,I,r,s,t,u,J,M,k,N,v,as,O,P,w,x,Q,R,S,T,U,V,W,o,aA,X,Y,Z,y,z,A,_,$,aa];ad(26,ab,"Z_syntax_plugin.Z_syntax");ad(27,[0,ab],"Z_syntax_plugin");return});
