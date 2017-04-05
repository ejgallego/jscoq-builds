(function(K){"use strict";var
b=K.jsoo_runtime,a=b.caml_new_string,t=b.caml_register_global,J=b.caml_wrap_exception;function
c(a,c){return a.length==1?a(c):b.caml_call_gen(a,[c])}function
h(a,c,d){return a.length==2?a(c,d):b.caml_call_gen(a,[c,d])}function
I(a,c,d,e){return a.length==3?a(c,d,e):b.caml_call_gen(a,[c,d,e])}function
s(a,c,d,e,f){return a.length==4?a(c,d,e,f):b.caml_call_gen(a,[c,d,e,f])}var
d=b.caml_get_global_data(),k=a("nat_syntax_plugin"),f=d.Coqlib,o=d.Globnames,g=d.Bigint,e=d.Pp,q=d.Loc,E=d.CErrors,B=d.CWarnings,H=d.Notation;c(d.Mltop[12],k);var
l=c(g[3],5e3),C=a("Cannot interpret a negative number as a number of type nat"),D=[0,a("nat_of_int")],v=a("limits and on the command executed)."),w=a("may vary from 5000 to 70000 depending on your system "),x=a("working with large numbers in nat (observed threshold "),y=a("Stack overflow or segmentation fault happens when "),z=a("numbers"),A=a("large-nat"),F=a("Nat_syntax.Non_closed_number"),G=a("nat_scope");function
u(j){var
a=c(e[22],v),b=c(e[22],w),d=c(e[22],x),f=c(e[22],y),g=h(e[12],f,d),i=h(e[12],g,b);return h(e[12],i,a)}var
m=s(B[2],A,z,0,u);function
n(d,a){if(c(g[20],a)){if(h(g[16],l,a))h(m,0,0);var
j=[0,[0,d,f[23],0]],i=a,k=[0,[0,d,f[24],0]];for(;;){if(b.caml_notequal(i,g[5])){var
j=[4,d,k,[0,j,0]],i=c(g[10],i);continue}return j}}var
n=c(e[3],C);return I(E[6],0,D,n)}var
i=[248,F,b.caml_fresh_oo_id(0)];function
j(a){switch(a[0]){case
0:if(h(o[5],a[1][2],f[23]))return g[5];break;case
4:var
d=a[2];if(0===d[0]){var
b=a[3];if(b)if(!b[2]){var
e=b[1];if(h(o[5],d[1][2],f[24])){var
k=j(e);return c(g[9],k)}}}break}throw i}function
p(a){try{var
b=[0,j(a)];return b}catch(a){a=J(a);if(a===i)return 0;throw a}}s(H[13],G,[0,f[19],f[18]],n,[0,[0,[0,[0,q[4],f[24],0]],[0,[0,[0,q[4],f[23],0]],0]],p,1]);var
r=[0,k,l,m,n,i,j,p];t(20,r,"Nat_syntax_plugin.Nat_syntax");t(21,[0,r],"Nat_syntax_plugin");return});
