(function(J){"use strict";var
d=J.jsoo_runtime,a=d.caml_new_string,t=d.caml_register_global,I=d.caml_wrap_exception;function
b(a,b){return a.length==1?a(b):d.caml_call_gen(a,[b])}function
h(a,b,c){return a.length==2?a(b,c):d.caml_call_gen(a,[b,c])}function
s(a,b,c,e,f){return a.length==4?a(b,c,e,f):d.caml_call_gen(a,[b,c,e,f])}var
c=d.caml_get_global_data(),k=a("nat_syntax_plugin"),f=c.Coqlib,o=c.Globnames,g=c.Bigint,e=c.Pp,q=c.Loc,E=c.CErrors,B=c.CWarnings,H=c.Notation;b(c.Mltop[12],k);var
l=b(g[3],5e3),C=a("Cannot interpret a negative number as a number of type nat"),D=a("nat_of_int"),v=a("limits and on the command executed)."),w=a("may vary from 5000 to 70000 depending on your system "),x=a("working with large numbers in nat (observed threshold "),y=a("Stack overflow or segmentation fault happens when "),z=a("numbers"),A=a("large-nat"),F=a("Nat_syntax.Non_closed_number"),G=a("nat_scope");function
u(j){var
a=b(e[25],v),c=b(e[25],w),d=b(e[25],x),f=b(e[25],y),g=h(e[13],f,d),i=h(e[13],g,c);return h(e[13],i,a)}var
m=s(B[2],A,z,0,u);function
n(a,c){if(b(g[20],c)){if(h(g[16],l,c))h(m,0,0);var
j=[0,[0,a,f[23],0]],i=c,k=[0,[0,a,f[24],0]];for(;;){if(d.caml_notequal(i,g[5])){var
j=[4,a,k,[0,j,0]],i=b(g[10],i);continue}return j}}var
n=[0,a,D,b(e[1],C)];return b(E[8],n)}var
i=[248,F,d.caml_fresh_oo_id(0)];function
j(a){switch(a[0]){case
0:if(h(o[5],a[1][2],f[23]))return g[5];break;case
4:var
d=a[2];if(0===d[0]){var
c=a[3];if(c)if(!c[2]){var
e=c[1];if(h(o[5],d[1][2],f[24])){var
k=j(e);return b(g[9],k)}}}break}throw i}function
p(a){try{var
b=[0,j(a)];return b}catch(a){a=I(a);if(a===i)return 0;throw a}}s(H[13],G,[0,f[19],f[18]],n,[0,[0,[0,[0,q[4],f[24],0]],[0,[0,[0,q[4],f[23],0]],0]],p,1]);var
r=[0,k,l,m,n,i,j,p];t(20,r,"Nat_syntax_plugin.Nat_syntax");t(21,[0,r],"Nat_syntax_plugin");return});
