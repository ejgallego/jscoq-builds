(function(bM){"use strict";var
J=123,r="plugins/quote/g_quote.ml4",az="in",A=250,av="$k",au="[",p=246,ay="$c",Q="Extension: cannot occur",t="quote",ax="]",at="$lc",s="Quote",P="$f",as="plugins/quote/quote.ml",aw="using",o=bM.jsoo_runtime,ar=o.caml_check_bound,c=o.caml_new_string,z=o.caml_obj_tag,ab=o.caml_register_global,$=o.caml_wrap_exception;function
a(a,b){return a.length==1?a(b):o.caml_call_gen(a,[b])}function
b(a,b,c){return a.length==2?a(b,c):o.caml_call_gen(a,[b,c])}function
i(a,b,c,d){return a.length==3?a(b,c,d):o.caml_call_gen(a,[b,c,d])}function
aa(a,b,c,d,e){return a.length==4?a(b,c,d,e):o.caml_call_gen(a,[b,c,d,e])}var
e=o.caml_get_global_data(),I=c("quote_plugin"),d=e.EConstr,f=e.Util,q=e.Tacmach,k=e.Proofview,m=e.Assert_failure,X=e.Tacticals,ao=e.Tactics,an=e.Constr_matching,h=e.Names,ad=e.Pp,ae=e.CErrors,ac=e.Coqlib,S=e.Termops,x=e.CamlinternalLazy,K=e.Term,aj=e.Option,ai=e.Patternops,w=e.Pervasives,g=e.Ltac_plugin,l=e.Loc,j=e.Stdarg,n=e.Genarg,ap=e.Mltop,a0=[0,c(as),503,15],aZ=[0,c(as),481,13],aX=c("invalid inversion scheme for quote."),aV=[0,c("Coq"),[0,c(t),[0,c(s),0]]],aQ=c("M"),aP=c("Quote: not a simple fixpoint"),aN=c("End_idx"),aO=[0,c(s),0],aL=c("Left_idx"),aM=[0,c(s),0],aJ=c("Right_idx"),aK=[0,c(s),0],aH=c("varmap_find"),aI=[0,c(s),0],aF=c("Node_vm"),aG=[0,c(s),0],aD=c("Empty_vm"),aE=[0,c(s),0],aA=c(t),aB=c(s),bJ=[0,c(r),1,0],bG=[0,c(r),1,0],bD=[0,c(r),1,0],bz=[0,c(r),1,0],bw=[0,c(r),1,0],bt=[0,c(r),1,0],bq=[0,c(r),1,0],bn=[0,c(r),1,0],bj=[0,c(r),1,0],bg=[0,c(r),1,0],bf=c(av),bh=[0,c(aw)],bi=c(ay),bk=[0,c(az)],bl=[0,c(ax)],bm=c(at),bo=[0,c(au)],bp=c(P),br=[0,c(t)],bs=c(av),bu=[0,c(aw)],bv=c(ay),bx=[0,c(az)],by=c(P),bA=[0,c(t)],bB=[0,[0,c(ax)],0],bC=c(at),bE=[0,c(au)],bF=c(P),bH=[0,c(t)],bI=c(P),bK=[0,c(t)],bL=c(t),a$=c(Q),a9=c(Q),a7=c(Q),a5=c(Q),a1=c("cont"),a2=c("x"),bd=c(t),aW=e.Not_found,aR=e.Global,aS=e.Environ,aT=e.Reductionops,aC=e.Universes,aU=e.Hashtbl,a3=e.Geninterp,bb=e.Array;function
u(c,b){var
e=i(ac[2],aB,[0,aA,c],b),f=a(aC[45],e);return a(d[8],f)}var
B=[p,function(a){return u(aE,aD)}],C=[p,function(a){return u(aG,aF)}],D=[p,function(a){return u(aI,aH)}],E=[p,function(a){return u(aK,aJ)}],F=[p,function(a){return u(aM,aL)}],G=[p,function(a){return u(aO,aN)}],L=a(f[20][1],[0,K[136]]);function
v(c){var
b=a(ad[3],aP);return i(ae[6],0,0,b)}function
R(a,c){var
e=b(S[61],a,c);return b(d[3],a,e)}function
af(c){var
b=a(h[1][8],c);return o.caml_int_of_string(i(f[15][4],b,1,o.caml_ml_string_length(b)-1|0))}function
ag(c){var
d=a(w[21],c),e=b(w[16],aQ,d);return a(h[1][6],e)}function
M(k,j,i,c){var
e=b(d[3],k,j);if(11===e[0]){var
g=e[1],h=g[1];if(0===h[2]){var
l=g[2],m=h[1],n=function(b){return a(d[11],c-b|0)},o=b(f[19][2],c,n),p=[0,a(d[28],[0,[0,[0,m,0],i+1|0],l]),o];return a(d[21],p)}}return v(0)}function
ah(j,c,e,m){function
k(n){var
g=n;for(;;){var
e=b(d[3],c,g);switch(e[0]){case
5:var
g=e[1];continue;case
9:var
l=e[2],h=e[1];if(b(d[44],c,h))if(b(d[64],c,h)===m){var
p=a(f[19][39],l);return[11,[0,ag(b(d[64],c,p))]]}var
q=b(f[19][15],k,l),r=b(d[5],c,h);return[4,i(ai[8],j,c,r),q];default:var
o=b(d[5],c,g);return i(ai[8],j,c,o)}}}return k(e)}function
T(G,F,o){var
r=a(k[63][5],o),c=a(q[48][4],o);try{var
_=b(d[73],c,G),g=_}catch(a){a=$(a);if(a!==K[27])throw a;var
g=v(0)}var
H=g[1],I=[0,H,b(d[2][2],c,g[2])],J=a(aR[2],0),N=b(aS[59],J,I),s=R(c,a(d[8],N));if(14===s[0]){var
t=s[1],u=t[1];if(1===u[1].length-1)if(0===u[2]){var
h=t[2];if(1===h[1].length-1)if(1===h[2].length-1){var
w=h[3];if(1===w.length-1){var
y=b(d[82],c,w[1]),e=y[1],O=y[2],P=a(f[17][1],e),Q=i(aT[81],0,r,c),j=R(c,O);if(13===j[0]){var
B=j[2],l=[0,0],m=[0,0],n=[0,0],T=j[4],U=function(i,v){var
o=b(d[82],c,v),g=o[2],h=a(f[17][1],o[1]);if(b(d[44],c,g))if(1===b(d[64],c,g)){n[1]=[0,M(c,a(f[17][5],e)[2],i,h)];return 0}var
q=b(d[81],c,g),s=q[2],w=q[1];if(s){var
t=s[2];if(t){var
j=t[2];if(j){var
k=j[2];if(k)if(!k[2]){var
C=k[1];if(b(d[44],c,j[1]))if(b(d[44],c,C)){var
u=z(D),E=A===u?D[1]:p===u?a(x[2],D):D;if(b(Q,w,E)){m[1]=[0,M(c,a(f[17][5],e)[2],i,h)];return 0}}}}}}var
y=l[1],B=ah(r,c,g,(P+h|0)+1|0);l[1]=[0,[0,M(c,a(f[17][5],e)[2],i,h),B],y];return 0};b(f[19][14],U,T);var
C=a(aj[3],n[1]),V=C?a(aj[3],m[1]):C;if(V)v(0);var
E=b(d[3],c,B),W=7===E[0]?a(S[49],E[3]):B,X=n[1],Y=i(f[17][19],L[4],F,L[1]),Z=m[1];return[0,a(f[17][9],l[1]),Z,W,Y,X]}return v(0)}}}}return v(0)}function
N(g,e,k){var
h=k;for(;;){var
l=a(d[J][1],h),i=b(L[3],l,e);if(i)return i;var
c=b(d[3],g,h);switch(c[0]){case
5:var
h=c[1];continue;case
9:var
m=c[2],j=N(g,e,c[1]);if(j){var
n=function(a){return N(g,e,a)};return b(f[19][31],n,m)}return j;default:return 0}}}function
ak(e,c){var
h=e.length-1,b=z(C),l=h>>>1|0,i=A===b?C[1]:p===b?a(x[2],C):C,j=z(B),k=[0,c],m=A===j?B[1]:p===j?a(x[2],B):B,f=a(d[21],[0,m,k]);function
g(b){if(h<b)return f;if(l<b){var
j=b-1|0,m=[0,i,[0,c,ar(e,j)[j+1],f,f]];return a(d[21],m)}var
n=g((2*b|0)+1|0),k=b-1|0,o=g(2*b|0),p=[0,i,[0,c,ar(e,k)[k+1],o,n]];return a(d[21],p)}return g(1)}function
al(e){function
b(a){return 1===a?0:[0,1===(a%2|0)?1:0,b(a>>>1|0)]}var
c=z(G),g=A===c?G[1]:p===c?a(x[2],G):G,h=b(e),j=a(f[17][9],h);function
k(g,f){var
h=[0,f];if(g)var
b=z(E),i=A===b?E[1]:p===b?a(x[2],E):E,c=i;else
var
e=z(F),j=A===e?F[1]:p===e?a(x[2],F):F,c=j;return a(d[21],[0,c,h])}return i(f[17][19],k,j,g)}function
U(c,k,h){var
e=k;for(;;){var
j=i(q[36],c,e,h);if(j)return j;var
l=a(q[2],c),g=b(d[3],l,e);switch(g[0]){case
5:var
e=g[1];continue;case
9:var
m=g[2],n=function(a){return U(c,a,h)};return b(f[19][29],n,m);default:return 0}}}function
am(e,b){var
h=a(q[2],e);function
f(a,b){if(b){var
g=b[2],c=b[1];return i(d[93],h,a,c)?b:U(e,a,c)?[0,a,[0,c,g]]:[0,c,f(a,g)]}return[0,a,0]}if(b){var
c=b[1];return f(c,am(e,b[2]))}return 0}var
O=a(aU[19],[0,K[131],K[147]]);function
H(e,c){function
g(b){var
c=b[1];return[0,c,a(d[J][1],b[2])]}var
h=b(f[17][15],g,e),i=a(d[J][1],c),j=b(S[47],h,i);return a(d[8],j)}function
V(t,k,e,c){a(ac[3],aV);var
l=a(O[1],17),j=[0,0],m=[0,1];function
n(c){function
g(u){var
g=u;for(;;){if(g){var
o=g[1],v=g[2],w=o[2],x=o[1];try{var
y=aa(an[3],t,k,w,c),z=a(h[1][11][17],y),A=function(a){var
b=a[1],c=n(a[2]);return[0,af(b),c]},B=H(b(f[17][15],A,z),x);return B}catch(a){a=$(a);if(a===an[1]){var
g=v;continue}throw a}}var
p=e[2];if(p){var
q=e[5],C=p[1];if(q){var
D=q[1];if(N(k,e[4],c))return H([0,[0,1,c],0],D)}try{var
F=a(d[J][1],c),G=b(O[7],l,F);return G}catch(b){b=$(b);if(b===aW){var
r=H([0,[0,1,al(m[1])],0],C);m[1]++;j[1]=[0,c,j[1]];var
E=a(d[J][1],c);i(O[5],l,E,r);return r}throw b}}var
s=e[5];if(s)return H([0,[0,1,c],0],s[1]);var
I=a(ad[3],aX);return i(ae[3],0,0,I)}}return g(e[1])}var
g=b(f[17][15],n,c),o=e[3],p=a(f[17][9],j[1]);return[0,g,ak(a(f[19][12],p),o)]}function
W(c){function
d(c,e){if(c){var
g=c[2],h=c[1],i=function(a){return d(g,[0,a,e])},j=a(X[66][58],h);return b(k[68][1],j,i)}var
l=a(f[17][9],e);return a(k[13],l)}return d(c,0)}function
aY(g,e){function
c(c){var
h=b(q[48][2],g,c);function
j(a){return b(q[48][2],a,c)}var
l=b(f[17][15],j,e);function
n(e){function
c(o){function
c(c){var
p=a(k[63][5],c),h=a(q[48][4],c),r=a(d[5],h),j=T(e,b(f[17][15],r,o),c),l=V(p,h,j,[0,a(k[63][3],c),0]),g=l[1];if(g)if(!g[2]){var
n=g[1],s=l[2];if(j[2]){var
t=a(d[21],[0,e,[0,s,n]]);return i(ao[3],0,t,2)}var
u=a(d[21],[0,e,[0,n]]);return i(ao[3],0,u,2)}throw[0,m,aZ]}return a(k[63][8],c)}var
g=W(l);return b(k[68][1],g,c)}var
o=a(X[66][58],h);return b(k[68][1],o,n)}return a(k[63][9],c)}var
y=[0,u,B,C,D,E,F,G,L,v,R,af,ag,M,ah,T,N,ak,al,U,am,O,H,V,W,aY,function(h,o,g,e){function
c(c){var
i=b(q[48][2],g,c);function
j(a){return b(q[48][2],a,c)}var
l=b(f[17][15],j,e);function
n(c){function
e(p){function
e(e){var
r=a(k[63][5],e),i=a(q[48][4],e),s=a(d[5],i),j=T(c,b(f[17][15],s,p),e),l=V(r,i,j,[0,o,0]),g=l[1];if(g)if(!g[2]){var
n=g[1],t=l[2];return j[2]?a(h,a(d[21],[0,c,[0,t,n]])):a(h,a(d[21],[0,c,[0,n]]))}throw[0,m,a0]}return a(k[63][8],e)}var
g=W(l);return b(k[68][1],g,e)}var
p=a(X[66][58],i);return b(k[68][1],p,n)}return a(k[63][9],c)}];ab(85,y,"Quote_plugin.Quote");a(ap[12],I);var
Y=a(h[1][6],a1),Z=a(h[1][6],a2);function
_(d,c){var
e=a(g[12][2][1],c),f=[0,[2,[1,b(l[10],0,Z)]],0],j=[0,[1,b(l[10],0,Y)],f],k=[3,b(l[10],0,j)],m=a3[4][1],n=b(h[1][11][5],Z,e),o=[0,i(h[1][11][4],Y,d,n),m],p=[29,b(l[10],0,k)];return b(g[12][22],o,p)}var
a4=0,a6=[0,function(c){if(c){var
d=c[2];if(d){var
e=d[2];if(e){var
f=e[2];if(f)if(!f[2]){var
h=f[1],i=e[1],k=d[1],l=c[1],m=a(n[6],j[9]),o=b(g[12][2][7],m,l),p=a(n[17],j[9]),q=a(n[6],p),r=b(g[12][2][7],q,k),s=a(n[6],j[13]),t=b(g[12][2][7],s,i),u=a(n[6],g[1][1]),v=b(g[12][2][7],u,h);return function(b){function
a(a){return _(v,a)}return aa(y[26],a,t,o,r)}}}}}return a(w[2],a5)},a4],a8=[0,function(c){if(c){var
d=c[2];if(d){var
e=d[2];if(e)if(!e[2]){var
f=e[1],h=d[1],i=c[1],k=a(n[6],j[9]),l=b(g[12][2][7],k,i),m=a(n[6],j[13]),o=b(g[12][2][7],m,h),p=a(n[6],g[1][1]),q=b(g[12][2][7],p,f);return function(c){var
a=0;function
b(a){return _(q,a)}return aa(y[26],b,o,l,a)}}}}return a(w[2],a7)},a6],a_=[0,function(c){if(c){var
d=c[2];if(d)if(!d[2]){var
e=d[1],f=c[1],h=a(n[6],j[9]),i=b(g[12][2][7],h,f),k=a(n[17],j[9]),l=a(n[6],k),m=b(g[12][2][7],l,e);return function(a){return b(y[25],i,m)}}}return a(w[2],a9)},a8],ba=[0,function(c){if(c)if(!c[2]){var
d=c[1],e=a(n[6],j[9]),f=b(g[12][2][7],e,d);return function(a){return b(y[25],f,0)}}return a(w[2],a$)},a_],bc=a(bb[12],ba);i(g[6][9],0,[0,I,bd],bc);function
be(R){var
u=[0,a(h[1][7],bf)],c=g[1][1],s=0,t=0;if(0===c[0]){var
v=[0,bh,[0,[1,b(l[10],0,[0,[5,[0,c[1]]],u])],t]],w=[0,a(h[1][7],bi)],d=j[13];if(0===d[0]){var
x=[0,bl,[0,bk,[0,[1,b(l[10],0,[0,[5,[0,d[1]]],w])],v]]],y=[0,a(h[1][7],bm)],e=j[9];if(0===e[0]){var
z=[0,bo,[0,[1,b(l[10],0,[0,[0,[5,[0,e[1]]]],y])],x]],A=[0,a(h[1][7],bp)],f=j[9];if(0===f[0]){var
B=[0,[0,br,[0,[1,b(l[10],0,[0,[5,[0,f[1]]],A])],z]],s],D=[0,a(h[1][7],bs)],k=g[1][1],C=0;if(0===k[0]){var
E=[0,bu,[0,[1,b(l[10],0,[0,[5,[0,k[1]]],D])],C]],F=[0,a(h[1][7],bv)],n=j[13];if(0===n[0]){var
G=[0,bx,[0,[1,b(l[10],0,[0,[5,[0,n[1]]],F])],E]],H=[0,a(h[1][7],by)],o=j[9];if(0===o[0]){var
J=[0,[0,bA,[0,[1,b(l[10],0,[0,[5,[0,o[1]]],H])],G]],B],K=[0,a(h[1][7],bC)],p=j[9];if(0===p[0]){var
L=[0,bE,[0,[1,b(l[10],0,[0,[0,[5,[0,p[1]]]],K])],bB]],M=[0,a(h[1][7],bF)],q=j[9];if(0===q[0]){var
N=[0,[0,bH,[0,[1,b(l[10],0,[0,[5,[0,q[1]]],M])],L]],J],P=[0,a(h[1][7],bI)],r=j[9],O=0;if(0===r[0]){var
Q=[0,[0,bK,[0,[1,b(l[10],0,[0,[5,[0,r[1]]],P])],O]],N];return i(g[9][4],[0,I,bL],0,Q)}throw[0,m,bJ]}throw[0,m,bG]}throw[0,m,bD]}throw[0,m,bz]}throw[0,m,bw]}throw[0,m,bt]}throw[0,m,bq]}throw[0,m,bn]}throw[0,m,bj]}throw[0,m,bg]}b(ap[19],be,I);var
aq=[0,I,Y,Z,_];ab(93,aq,"Quote_plugin.G_quote");ab(94,[0,y,aq],"Quote_plugin");return});
