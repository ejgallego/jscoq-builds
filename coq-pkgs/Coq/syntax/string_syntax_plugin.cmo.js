(function(Q){"use strict";var
r="String",n=250,h=246,E="string",D="Strings",C="Coq",b=Q.jsoo_runtime,a=b.caml_new_string,m=b.caml_obj_tag,B=b.caml_register_global,O=b.caml_wrap_exception;function
f(a,c){return a.length==1?a(c):b.caml_call_gen(a,[c])}function
g(a,c,d){return a.length==2?a(c,d):b.caml_call_gen(a,[c,d])}function
N(a,c,d,e){return a.length==3?a(c,d,e):b.caml_call_gen(a,[c,d,e])}function
P(a,c,d,e,f){return a.length==4?a(c,d,e,f):b.caml_call_gen(a,[c,d,e,f])}var
e=b.caml_get_global_data(),u=a("string_syntax_plugin"),l=[0,a(C),[0,a(D),[0,a(r),0]]],i=e.CamlinternalLazy,t=e.Globnames,o=e.Buffer,j=e.Ascii_syntax_plugin,s=e.Loc,H=e.Char,G=e.Coqlib,F=e.Notation;f(e.Mltop[12],u);var
k=[248,a("String_syntax.Non_closed_string"),b.caml_fresh_oo_id(0)],v=g(j[1][5],l,a(E)),p=g(j[1][4],l,a(E)),w=[3,[0,[0,p,0],1]],x=[3,[0,[0,p,0],2]],K=a("EmptyString"),J=a(r),I=a("String interpretation"),L=[0,a(C),[0,a(D),[0,a(r),0]]],M=a("string_scope");function
q(a){return N(G[1],I,l,a)}var
c=[h,function(a){return q(J)}],d=[h,function(a){return q(K)}];function
y(a,k){var
q=b.caml_ml_string_length(k);function
l(e){if(e===q){var
o=m(d),r=0,s=n===o?d[1]:h===o?f(i[2],d):d;return[0,[0,a,s,r]]}var
t=[0,l(e+1|0),0],u=b.caml_string_get(k,e),p=m(c),v=[0,g(j[1][13],a,u),t],w=0,x=n===p?c[1]:h===p?f(i[2],c):c;return[4,a,[0,[0,a,x,w]],v]}return l(0)}function
z(a){try{var
l=f(o[1],16),b=function(u){var
a=u;for(;;){switch(a[0]){case
0:var
p=m(d),v=a[1][2],w=n===p?d[1]:h===p?f(i[2],d):d;if(g(t[5],v,w))return[0,f(o[2],l)];break;case
4:var
q=a[2];if(0===q[0]){var
b=a[3];if(b){var
e=b[2];if(e)if(!e[2]){var
r=m(c),x=e[1],y=b[1],z=q[1][2],A=n===r?c[1]:h===r?f(i[2],c):c;if(g(t[5],z,A)){var
s=f(j[1][15],y);if(s){var
B=f(H[1],s[1]);g(o[10],l,B);var
a=x;continue}throw k}}}}break}throw k}}(a);return b}catch(a){a=O(a);if(a===k)return 0;throw a}}P(F[14],M,[0,v,L],y,[0,[0,[0,[0,s[4],x,0]],[0,[0,[0,s[4],w,0]],0]],z,1]);var
A=[0,u,k,l,v,p,w,x,q,c,d,y,z];B(19,A,"String_syntax_plugin.String_syntax");B(20,[0,A],"String_syntax_plugin");return});
