(function(J){"use strict";var
d=J.jsoo_runtime,a=d.caml_new_string,t=d.caml_register_global,I=d.caml_wrap_exception;function
b(a,b){return a.length==1?a(b):d.caml_call_gen(a,[b])}function
h(a,b,c){return a.length==2?a(b,c):d.caml_call_gen(a,[b,c])}function
s(a,b,c,e,f){return a.length==4?a(b,c,e,f):d.caml_call_gen(a,[b,c,e,f])}var
c=d.caml_get_global_data(),m=a("nat_syntax_plugin"),f=c.Coqlib,l=c.Globnames,g=c.Bigint,e=c.Pp,k=c.Loc,w=c.CErrors,u=c.Notation,v=c.CWarnings;b(c.Mltop[12],m);var
n=b(g[3],5e3),E=a("Cannot interpret a negative number as a number of type nat"),F=a("nat_of_int"),y=a("limits and on the command executed)."),z=a("may vary from 5000 to 70000 depending on your system "),A=a("working with large numbers in nat (observed threshold "),B=a("Stack overflow or segmentation fault happens when "),C=a("numbers"),D=a("large-nat"),G=a("Nat_syntax.Non_closed_number"),H=a("nat_scope");function
x(j){var
a=b(e[25],y),c=b(e[25],z),d=b(e[25],A),f=b(e[25],B),g=h(e[13],f,d),i=h(e[13],g,c);return h(e[13],i,a)}var
o=s(v[2],D,C,0,x);function
p(a,c){if(b(g[20],c)){if(h(g[16],n,c))h(o,0,0);var
j=[0,[0,a,f[23],0]],i=c,k=[0,[0,a,f[24],0]];for(;;){if(d.caml_notequal(i,g[5])){var
j=[4,a,k,[0,j,0]],i=b(g[10],i);continue}return j}}var
l=[0,a,F,b(e[1],E)];return b(w[8],l)}var
i=[248,G,d.caml_fresh_oo_id(0)];function
j(a){switch(a[0]){case
0:if(h(l[5],a[1][2],f[23]))return g[5];break;case
4:var
d=a[2];if(0===d[0]){var
c=a[3];if(c)if(!c[2]){var
e=c[1];if(h(l[5],d[1][2],f[24])){var
k=j(e);return b(g[9],k)}}}break}throw i}function
q(a){try{var
b=[0,j(a)];return b}catch(a){a=I(a);if(a===i)return 0;throw a}}s(u[13],H,[0,f[19],f[18]],p,[0,[0,[0,[0,k[4],f[24],0]],[0,[0,[0,k[4],f[23],0]],0]],q,1]);var
r=[0,m,n,o,p,i,j,q];t(20,r,"Nat_syntax_plugin.Nat_syntax");t(21,[0,r],"Nat_syntax_plugin");return});
