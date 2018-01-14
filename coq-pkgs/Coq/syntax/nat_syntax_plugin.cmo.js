(function(M){"use strict";var
c=M.jsoo_runtime,b=c.caml_new_string,t=c.caml_register_global,L=c.caml_wrap_exception;function
d(a,b){return a.length==1?a(b):c.caml_call_gen(a,[b])}function
a(a,b,d){return a.length==2?a(b,d):c.caml_call_gen(a,[b,d])}function
K(a,b,d,e){return a.length==3?a(b,d,e):c.caml_call_gen(a,[b,d,e])}function
s(a,b,d,e,f){return a.length==4?a(b,d,e,f):c.caml_call_gen(a,[b,d,e,f])}var
e=c.caml_get_global_data(),l=b("nat_syntax_plugin"),g=e.Coqlib,p=e.Globnames,h=e.Bigint,i=e.CAst,f=e.Pp,E=e.CErrors,B=e.CWarnings,J=e.Notation;d(e.Mltop[12],l);var
m=d(h[3],5e3),C=b("Cannot interpret a negative number as a number of type nat"),D=[0,b("nat_of_int")],v=b("limits and on the command executed)."),w=b("may vary from 5000 to 70000 depending on your system "),x=b("working with large numbers in nat (observed threshold "),y=b("Stack overflow or segmentation fault happens when "),z=b("numbers"),A=b("large-nat"),F=b("Nat_syntax.Non_closed_number"),I=b("nat_scope");function
u(j){var
b=d(f[22],v),c=d(f[22],w),e=d(f[22],x),g=d(f[22],y),h=a(f[12],g,e),i=a(f[12],h,c);return a(f[12],i,b)}var
n=s(B[2],A,z,0,u);function
o(b,e){if(d(h[20],e)){if(a(h[16],m,e))a(n,0,0);var
l=a(i[1],b,[0,g[21],0]),k=l,j=e,o=a(i[1],b,[0,g[22],0]);for(;;){if(c.caml_notequal(j,h[5])){var
p=d(h[10],j),k=a(i[1],b,[4,o,[0,k,0]]),j=p;continue}return k}}var
q=d(f[3],C);return K(E[6],b,D,q)}var
j=[248,F,c.caml_fresh_oo_id(0)];function
k(b){function
c(b){switch(b[0]){case
0:if(a(p[5],b[1],g[21]))return h[5];break;case
4:var
e=b[1][1];if(0===e[0]){var
c=b[2];if(c)if(!c[2]){var
f=c[1];if(a(p[5],e[1],g[22])){var
i=k(f);return d(h[9],i)}}}break}throw j}return a(i[5],c,b)}function
q(a){try{var
b=[0,k(a)];return b}catch(a){a=L(a);if(a===j)return 0;throw a}}var
G=[0,a(i[1],0,[0,g[21],0]),0],H=[0,[0,a(i[1],0,[0,g[22],0]),G],q,1];s(J[14],I,[0,g[17],g[14]],o,H);var
r=[0,l,m,n,o,j,k,q];t(20,r,"Nat_syntax_plugin.Nat_syntax");t(21,[0,r],"Nat_syntax_plugin");return});
