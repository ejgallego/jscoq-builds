(function(Q){"use strict";var
r="String",n=250,h=246,E="string",D="Strings",C="Coq",b=Q.jsoo_runtime,a=b.caml_new_string,m=b.caml_obj_tag,B=b.caml_register_global,O=b.caml_wrap_exception;function
f(a,c){return a.length==1?a(c):b.caml_call_gen(a,[c])}function
g(a,c,d){return a.length==2?a(c,d):b.caml_call_gen(a,[c,d])}function
N(a,c,d,e){return a.length==3?a(c,d,e):b.caml_call_gen(a,[c,d,e])}function
P(a,c,d,e,f){return a.length==4?a(c,d,e,f):b.caml_call_gen(a,[c,d,e,f])}var
e=b.caml_get_global_data(),s=a("string_syntax_plugin"),j=[0,a(C),[0,a(D),[0,a(r),0]]],l=e.CamlinternalLazy,y=e.Globnames,q=e.Buffer,k=e.Ascii_syntax_plugin,z=e.Loc,J=e.Char,G=e.Coqlib,M=e.Notation;f(e.Mltop[12],s);var
i=[248,a("String_syntax.Non_closed_string"),b.caml_fresh_oo_id(0)],t=g(k[1][5],j,a(E)),o=g(k[1][4],j,a(E)),u=[3,[0,[0,o,0],1]],v=[3,[0,[0,o,0],2]],I=a("EmptyString"),H=a(r),F=a("String interpretation"),K=[0,a(C),[0,a(D),[0,a(r),0]]],L=a("string_scope");function
p(a){return N(G[1],F,j,a)}var
c=[h,function(a){return p(H)}],d=[h,function(a){return p(I)}];function
w(a,i){var
q=b.caml_ml_string_length(i);function
j(e){if(e===q){var
o=m(d),r=0,s=n===o?d[1]:h===o?f(l[2],d):d;return[0,[0,a,s,r]]}var
t=[0,j(e+1|0),0],u=b.caml_string_get(i,e),p=m(c),v=[0,g(k[1][13],a,u),t],w=0,x=n===p?c[1]:h===p?f(l[2],c):c;return[4,a,[0,[0,a,x,w]],v]}return j(0)}function
x(a){try{var
j=f(q[1],16),b=function(t){var
a=t;for(;;){switch(a[0]){case
0:var
o=m(d),u=a[1][2],v=n===o?d[1]:h===o?f(l[2],d):d;if(g(y[5],u,v))return[0,f(q[2],j)];break;case
4:var
p=a[2];if(0===p[0]){var
b=a[3];if(b){var
e=b[2];if(e)if(!e[2]){var
r=m(c),w=e[1],x=b[1],z=p[1][2],A=n===r?c[1]:h===r?f(l[2],c):c;if(g(y[5],z,A)){var
s=f(k[1][15],x);if(s){var
B=f(J[1],s[1]);g(q[10],j,B);var
a=w;continue}throw i}}}}break}throw i}}(a);return b}catch(a){a=O(a);if(a===i)return 0;throw a}}P(M[14],L,[0,t,K],w,[0,[0,[0,[0,z[4],v,0]],[0,[0,[0,z[4],u,0]],0]],x,1]);var
A=[0,s,i,j,t,o,u,v,p,c,d,w,x];B(19,A,"String_syntax_plugin.String_syntax");B(20,[0,A],"String_syntax_plugin");return});
