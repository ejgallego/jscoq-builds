function(bX){"use strict";var
ax="in",w=250,at="$k",F=127,as="[",l=246,aw="$c",r="quote",av="]",ar="$lc",p="Quote",M="$f",aq="plugins/quote/quote.ml",au="using",k=bX.jsoo_runtime,ap=k.caml_check_bound,c=k.caml_new_string,v=k.caml_obj_tag,Z=k.caml_register_global,Y=k.caml_wrap_exception;function
a(a,b){return a.length==1?a(b):k.caml_call_gen(a,[b])}function
b(a,b,c){return a.length==2?a(b,c):k.caml_call_gen(a,[b,c])}function
h(a,b,c,d){return a.length==3?a(b,c,d):k.caml_call_gen(a,[b,c,d])}function
L(a,b,c,d,e){return a.length==4?a(b,c,d,e):k.caml_call_gen(a,[b,c,d,e])}var
e=k.caml_get_global_data(),U=c("quote_plugin"),d=e.EConstr,f=e.Util,m=e.Tacmach,i=e.Proofview,al=e.Assert_failure,T=e.Tacticals,am=e.Tactics,ak=e.Constr_matching,g=e.Names,$=e.Pp,aa=e.CErrors,_=e.Coqlib,O=e.Termops,t=e.CamlinternalLazy,G=e.Constr,ag=e.Option,af=e.Patternops,ad=e.Pervasives,E=e.Ltac_plugin,an=e.CAst,j=e.Loc,n=e.Genarg,o=e.Stdarg,aY=[0,c(aq),497,15],aX=[0,c(aq),475,13],aV=c("invalid inversion scheme for quote."),aT=[0,c("Coq"),[0,c(r),[0,c(p),0]]],aO=c("M"),aN=c("Quote: not a simple fixpoint"),aL=c("End_idx"),aM=[0,c(p),0],aJ=c("Left_idx"),aK=[0,c(p),0],aH=c("Right_idx"),aI=[0,c(p),0],aF=c("varmap_find"),aG=[0,c(p),0],aD=c("Node_vm"),aE=[0,c(p),0],aB=c("Empty_vm"),aC=[0,c(p),0],ay=c(r),az=c(p),a0=c("cont"),a1=c("x"),a5=c(at),a8=c(au),a_=c(aw),bb=c(ax),bc=c(av),be=c(ar),bh=c(as),bj=c(M),bm=c(r),bp=c(at),bs=c(au),bu=c(aw),bx=c(ax),bz=c(M),bC=c(r),bF=[0,c(av),0],bG=c(ar),bJ=c(as),bL=c(M),bO=c(r),bR=c(M),bU=c(r),bW=c(r),aU=e.Not_found,aP=e.Global,aQ=e.Environ,aR=e.Reductionops,aA=e.Universes,aS=e.Hashtbl,a2=e.Geninterp,aZ=e.Mltop;function
q(c,b){var
e=h(_[2],az,[0,ay,c],b),f=a(aA[50],e);return a(d[8],f)}var
x=[l,function(a){return q(aC,aB)}],y=[l,function(a){return q(aE,aD)}],z=[l,function(a){return q(aG,aF)}],A=[l,function(a){return q(aI,aH)}],B=[l,function(a){return q(aK,aJ)}],C=[l,function(a){return q(aM,aL)}],H=a(f[20][1],[0,G[80]]);function
s(c){var
b=a($[3],aN);return h(aa[6],0,0,b)}function
N(a,c){var
e=b(O[60],a,c);return b(d[3],a,e)}function
ab(c){var
b=a(g[1][8],c);return k.caml_int_of_string(h(f[15][4],b,1,k.caml_ml_string_length(b)-1|0))}function
ac(c){var
d=a(ad[21],c),e=b(ad[16],aO,d);return a(g[1][6],e)}function
I(k,j,i,c){var
e=b(d[3],k,j);if(11===e[0]){var
g=e[1],h=g[1];if(0===h[2]){var
l=g[2],m=h[1],n=function(b){return a(d[11],c-b|0)},o=b(f[19][2],c,n),p=[0,a(d[28],[0,[0,[0,m,0],i+1|0],l]),o];return a(d[21],p)}}return s(0)}function
ae(j,c,e,m){function
k(n){var
g=n;for(;;){var
e=b(d[3],c,g);switch(e[0]){case
5:var
g=e[1];continue;case
9:var
l=e[2],i=e[1];if(b(d[44],c,i))if(b(d[65],c,i)===m){var
p=a(f[19][39],l);return[11,[0,ac(b(d[65],c,p))]]}var
q=b(f[19][15],k,l),r=b(d[5],c,i);return[4,h(af[8],j,c,r),q];default:var
o=b(d[5],c,g);return h(af[8],j,c,o)}}}return k(e)}function
P(J,F,q){var
r=a(i[66][5],q),c=a(m[42][4],q);try{var
$=b(d[74],c,J),g=$}catch(a){a=Y(a);if(a!==G[54])throw a;var
g=s(0)}var
K=g[1],L=[0,K,b(d[2][2],c,g[2])],M=a(aP[2],0),P=b(aQ[55],M,L),u=N(c,a(d[8],P));if(14===u[0]){var
x=u[1],y=x[1];if(1===y[1].length-1)if(0===y[2]){var
j=x[2];if(1===j[1].length-1)if(1===j[2].length-1){var
A=j[3];if(1===A.length-1){var
B=b(d[83],c,A[1]),e=B[1],Q=B[2],R=a(f[17][1],e),S=h(aR[79],0,r,c),k=N(c,Q);if(13===k[0]){var
C=k[2],n=[0,0],o=[0,0],p=[0,0],T=k[4],U=function(i,y){var
m=b(d[83],c,y),g=m[2],h=a(f[17][1],m[1]);if(b(d[44],c,g))if(1===b(d[65],c,g)){p[1]=[0,I(c,a(f[17][5],e)[2],i,h)];return 0}var
q=b(d[82],c,g),s=q[2],A=q[1];if(s){var
u=s[2];if(u){var
j=u[2];if(j){var
k=j[2];if(k)if(!k[2]){var
D=k[1];if(b(d[44],c,j[1]))if(b(d[44],c,D)){var
x=v(z),E=w===x?z[1]:l===x?a(t[2],z):z;if(b(S,A,E)){o[1]=[0,I(c,a(f[17][5],e)[2],i,h)];return 0}}}}}}var
B=n[1],C=ae(r,c,g,(R+h|0)+1|0);n[1]=[0,[0,I(c,a(f[17][5],e)[2],i,h),C],B];return 0};b(f[19][14],U,T);var
D=a(ag[3],p[1]),V=D?a(ag[3],o[1]):D;if(V)s(0);var
E=b(d[3],c,C),W=7===E[0]?a(O[47],E[3]):C,X=p[1],Z=h(f[17][19],H[4],F,H[1]),_=o[1];return[0,a(f[17][9],n[1]),_,W,Z,X]}return s(0)}}}}return s(0)}function
J(g,e,k){var
h=k;for(;;){var
l=a(d[F][1],h),i=b(H[3],l,e);if(i)return i;var
c=b(d[3],g,h);switch(c[0]){case
5:var
h=c[1];continue;case
9:var
m=c[2],j=J(g,e,c[1]);if(j){var
n=function(a){return J(g,e,a)};return b(f[19][31],n,m)}return j;default:return 0}}}function
ah(e,c){var
h=e.length-1,b=v(y),m=h>>>1|0,i=w===b?y[1]:l===b?a(t[2],y):y,j=v(x),k=[0,c],n=w===j?x[1]:l===j?a(t[2],x):x,f=a(d[21],[0,n,k]);function
g(b){if(h<b)return f;if(m<b){var
j=b-1|0,l=[0,i,[0,c,ap(e,j)[j+1],f,f]];return a(d[21],l)}var
n=g((2*b|0)+1|0),k=b-1|0,o=g(2*b|0),p=[0,i,[0,c,ap(e,k)[k+1],o,n]];return a(d[21],p)}return g(1)}function
ai(e){function
b(a){return 1===a?0:[0,1===(a%2|0)?1:0,b(a>>>1|0)]}var
c=v(C),g=w===c?C[1]:l===c?a(t[2],C):C,i=b(e),j=a(f[17][9],i);function
k(g,f){var
h=[0,f];if(g)var
b=v(A),i=w===b?A[1]:l===b?a(t[2],A):A,c=i;else
var
e=v(B),j=w===e?B[1]:l===e?a(t[2],B):B,c=j;return a(d[21],[0,c,h])}return h(f[17][19],k,j,g)}function
Q(c,k,i){var
e=k;for(;;){var
j=h(m[35],c,e,i);if(j)return j;var
l=a(m[2],c),g=b(d[3],l,e);switch(g[0]){case
5:var
e=g[1];continue;case
9:var
n=g[2],o=function(a){return Q(c,a,i)};return b(f[19][29],o,n);default:return 0}}}function
aj(e,b){var
i=a(m[2],e);function
f(a,b){if(b){var
g=b[2],c=b[1];return h(d[94],i,a,c)?b:Q(e,a,c)?[0,a,[0,c,g]]:[0,c,f(a,g)]}return[0,a,0]}if(b){var
c=b[1];return f(c,aj(e,b[2]))}return 0}var
K=a(aS[19],[0,G[74],G[92]]);function
D(e,c){function
g(b){var
c=b[1];return[0,c,a(d[F][1],b[2])]}var
h=b(f[17][15],g,e),i=a(d[F][1],c),j=b(O[45],h,i);return a(d[8],j)}function
R(t,k,e,c){a(_[3],aT);var
l=a(K[1],17),j=[0,0],m=[0,1];function
n(c){function
i(u){var
i=u;for(;;){if(i){var
o=i[1],v=i[2],w=o[2],x=o[1];try{var
y=L(ak[3],t,k,w,c),z=a(g[1][11][17],y),A=function(a){var
b=a[1],c=n(a[2]);return[0,ab(b),c]},B=D(b(f[17][15],A,z),x);return B}catch(a){a=Y(a);if(a===ak[1]){var
i=v;continue}throw a}}var
p=e[2];if(p){var
q=e[5],C=p[1];if(q){var
E=q[1];if(J(k,e[4],c))return D([0,[0,1,c],0],E)}try{var
H=a(d[F][1],c),I=b(K[7],l,H);return I}catch(b){b=Y(b);if(b===aU){var
r=D([0,[0,1,ai(m[1])],0],C);m[1]++;j[1]=[0,c,j[1]];var
G=a(d[F][1],c);h(K[5],l,G,r);return r}throw b}}var
s=e[5];if(s)return D([0,[0,1,c],0],s[1]);var
M=a($[3],aV);return h(aa[3],0,0,M)}}return i(e[1])}var
i=b(f[17][15],n,c),o=e[3],p=a(f[17][9],j[1]);return[0,i,ah(a(f[19][12],p),o)]}function
S(c){function
d(c,e){if(c){var
g=c[2],h=c[1],j=function(a){return d(g,[0,a,e])},k=a(T[66][59],h);return b(i[71][1],k,j)}var
l=a(f[17][9],e);return a(i[16],l)}return d(c,0)}function
aW(g,e){function
c(c){var
j=b(m[42][2],g,c);function
k(a){return b(m[42][2],a,c)}var
l=b(f[17][15],k,e);function
n(e){function
c(o){function
c(c){var
p=a(i[66][5],c),j=a(m[42][4],c),q=a(d[5],j),k=P(e,b(f[17][15],q,o),c),l=R(p,j,k,[0,a(i[66][3],c),0]),g=l[1];if(g)if(!g[2]){var
n=g[1],r=l[2];if(k[2]){var
s=a(d[21],[0,e,[0,r,n]]);return h(am[3],0,s,2)}var
t=a(d[21],[0,e,[0,n]]);return h(am[3],0,t,2)}throw[0,al,aX]}return a(i[66][9],c)}var
g=S(l);return b(i[71][1],g,c)}var
o=a(T[66][59],j);return b(i[71][1],o,n)}return a(i[66][10],c)}var
u=[0,q,x,y,z,A,B,C,H,s,N,ab,ac,I,ae,P,J,ah,ai,Q,aj,K,D,R,S,aW,function(h,o,g,e){function
c(c){var
j=b(m[42][2],g,c);function
k(a){return b(m[42][2],a,c)}var
l=b(f[17][15],k,e);function
n(c){function
e(p){function
e(e){var
q=a(i[66][5],e),j=a(m[42][4],e),r=a(d[5],j),k=P(c,b(f[17][15],r,p),e),l=R(q,j,k,[0,o,0]),g=l[1];if(g)if(!g[2]){var
n=g[1],s=l[2];return k[2]?a(h,a(d[21],[0,c,[0,s,n]])):a(h,a(d[21],[0,c,[0,n]]))}throw[0,al,aY]}return a(i[66][9],e)}var
g=S(l);return b(i[71][1],g,e)}var
p=a(T[66][59],j);return b(i[71][1],p,n)}return a(i[66][10],c)}];Z(70,u,"Quote_plugin.Quote");a(aZ[10],U);var
V=a(g[1][6],a0),W=a(g[1][6],a1);function
X(d,c){var
e=a(E[13][2][1],c),f=[0,[2,[1,b(an[1],0,W)]],0],i=[0,[1,b(an[1],0,V)],f],k=[3,b(j[11],0,i)],l=a2[5][1],m=b(g[1][11][5],W,e),n=[0,h(g[1][11][4],V,d,m),l],o=[29,b(j[11],0,k)];return b(E[13][23],n,o)}var
a3=0;function
a4(d,c,b,a,f){function
e(b){return X(a,b)}return L(u[26],e,b,d,c)}var
a6=a(g[1][7],a5),a7=[0,[5,a(n[16],E[2][1])],a6],a9=[0,a8,[1,b(j[11],0,a7),0]],a$=a(g[1][7],a_),ba=[0,[5,a(n[16],o[13])],a$],bd=[0,bc,[0,bb,[1,b(j[11],0,ba),a9]]],bf=a(g[1][7],be),bg=[0,[0,[5,a(n[16],o[8])]],bf],bi=[0,bh,[1,b(j[11],0,bg),bd]],bk=a(g[1][7],bj),bl=[0,[5,a(n[16],o[8])],bk],bn=[0,[0,[0,bm,[1,b(j[11],0,bl),bi]],a4],a3];function
bo(c,b,a,f){var
d=0;function
e(b){return X(a,b)}return L(u[26],e,b,c,d)}var
bq=a(g[1][7],bp),br=[0,[5,a(n[16],E[2][1])],bq],bt=[0,bs,[1,b(j[11],0,br),0]],bv=a(g[1][7],bu),bw=[0,[5,a(n[16],o[13])],bv],by=[0,bx,[1,b(j[11],0,bw),bt]],bA=a(g[1][7],bz),bB=[0,[5,a(n[16],o[8])],bA],bD=[0,[0,[0,bC,[1,b(j[11],0,bB),by]],bo],bn];function
bE(c,a,d){return b(u[25],c,a)}var
bH=a(g[1][7],bG),bI=[0,[0,[5,a(n[16],o[8])]],bH],bK=[0,bJ,[1,b(j[11],0,bI),bF]],bM=a(g[1][7],bL),bN=[0,[5,a(n[16],o[8])],bM],bP=[0,[0,[0,bO,[1,b(j[11],0,bN),bK]],bE],bD];function
bQ(a,c){return b(u[25],a,0)}var
bS=a(g[1][7],bR),bT=[0,[5,a(n[16],o[8])],bS],bV=[0,[0,[0,bU,[1,b(j[11],0,bT),0]],bQ],bP];L(E[10][8],U,bW,0,bV);var
ao=[0,U,V,W,X];Z(78,ao,"Quote_plugin.G_quote");Z(79,[0,u,ao],"Quote_plugin");return}
