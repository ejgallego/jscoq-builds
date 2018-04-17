function(M){"use strict";var
d=M.jsoo_runtime,b=d.caml_new_string,t=d.caml_register_global,L=d.caml_wrap_exception;function
c(a,b){return a.length==1?a(b):d.caml_call_gen(a,[b])}function
a(a,b,c){return a.length==2?a(b,c):d.caml_call_gen(a,[b,c])}function
K(a,b,c,e){return a.length==3?a(b,c,e):d.caml_call_gen(a,[b,c,e])}function
s(a,b,c,e,f){return a.length==4?a(b,c,e,f):d.caml_call_gen(a,[b,c,e,f])}var
e=d.caml_get_global_data(),l=b("nat_syntax_plugin"),g=e.Coqlib,p=e.Globnames,h=e.Bigint,i=e.DAst,f=e.Pp,E=e.CErrors,B=e.CWarnings,J=e.Notation;c(e.Mltop[10],l);var
m=c(h[3],5e3),C=b("Cannot interpret a negative number as a number of type nat"),D=[0,b("nat_of_int")],v=b("limits and on the command executed)."),w=b("may vary from 5000 to 70000 depending on your system "),x=b("working with large numbers in nat (observed threshold "),y=b("Stack overflow or segmentation fault happens when "),z=b("numbers"),A=b("large-nat"),F=b("Nat_syntax.Non_closed_number"),I=b("nat_scope");function
u(j){var
b=c(f[22],v),d=c(f[22],w),e=c(f[22],x),g=c(f[22],y),h=a(f[12],g,e),i=a(f[12],h,d);return a(f[12],i,b)}var
n=s(B[1],A,z,0,u);function
o(b,e){if(c(h[20],e)){if(a(h[16],m,e))a(n,0,0);var
l=a(i[3],b,[0,g[21],0]),k=l,j=e,o=a(i[3],b,[0,g[22],0]);for(;;){if(d.caml_notequal(j,h[5])){var
p=c(h[10],j),k=a(i[3],b,[4,o,[0,k,0]]),j=p;continue}return k}}var
q=c(f[3],C);return K(E[6],b,D,q)}var
j=[248,F,d.caml_fresh_oo_id(0)];function
k(b){function
d(b){switch(b[0]){case
0:if(a(p[5],b[1],g[21]))return h[5];break;case
4:var
d=b[2];if(d)if(!d[2]){var
f=d[1],e=c(i[1],b[1]);if(0===e[0])if(a(p[5],e[1],g[22])){var
l=k(f);return c(h[9],l)}throw j}break}throw j}return a(i[8],d,b)}function
q(a){var
b=a[1];try{var
c=[0,k(b)];return c}catch(a){a=L(a);if(a===j)return 0;throw a}}var
G=[0,a(i[3],0,[0,g[21],0]),0],H=[0,[0,a(i[3],0,[0,g[22],0]),G],q,1];s(J[14],I,[0,g[17],g[14]],o,H);var
r=[0,l,m,n,o,j,k,q];t(20,r,"Nat_syntax_plugin.Nat_syntax");t(21,[0,r],"Nat_syntax_plugin");return}
