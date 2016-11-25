(function(bP){"use strict";var
B=140,p="plugins/quote/g_quote.ml4",aA="in",A=250,av="quote_plugin",q=124,au="$k",at="[",n=246,az="$c",S="Extension: cannot occur",s="quote",ay="]",as="$lc",r="Quote",R="$f",ar="plugins/quote/quote.ml",ax=136,aw="using",m=bP.jsoo_runtime,aq=m.caml_check_bound,b=m.caml_new_string,z=m.caml_obj_tag,ab=m.caml_register_global,$=m.caml_wrap_exception;function
a(a,b){return a.length==1?a(b):m.caml_call_gen(a,[b])}function
c(a,b,c){return a.length==2?a(b,c):m.caml_call_gen(a,[b,c])}function
i(a,b,c,d){return a.length==3?a(b,c,d):m.caml_call_gen(a,[b,c,d])}function
aa(a,b,c,d,e){return a.length==4?a(b,c,d,e):m.caml_call_gen(a,[b,c,d,e])}var
e=m.caml_get_global_data(),Q=b(av),d=e.Term,f=e.Util,u=e.Tacmach,j=e.Assert_failure,K=e.Proofview,ac=e.Tactics,T=e.Evd,L=e.Global,ag=e.Constr_matching,h=e.Names,C=e.Termops,ah=e.CErrors,af=e.Coqlib,w=e.CamlinternalLazy,ae=e.Option,ad=e.Patternops,v=e.Pervasives,g=e.Constrarg,o=e.Loc,l=e.Genarg,k=e.Tacinterp,ao=e.Mltop,a0=[0,b(ar),473,11],aZ=[0,b(ar),458,13],aX=b("invalid inversion scheme for quote"),aW=[0,b("Coq"),[0,b(s),[0,b(r),0]]],aV=b("M"),aU=b("Quote: not a simple fixpoint"),aS=b("End_idx"),aT=[0,b(r),0],aQ=b("Left_idx"),aR=[0,b(r),0],aO=b("Right_idx"),aP=[0,b(r),0],aM=b("varmap_find"),aN=[0,b(r),0],aK=b("Node_vm"),aL=[0,b(r),0],aI=b("Empty_vm"),aJ=[0,b(r),0],aG=b(s),aH=b(r),bM=[0,b(p),1,0],bJ=[0,b(p),1,0],bG=[0,b(p),1,0],bC=[0,b(p),1,0],bz=[0,b(p),1,0],bw=[0,b(p),1,0],bt=[0,b(p),1,0],bq=[0,b(p),1,0],bm=[0,b(p),1,0],bj=[0,b(p),1,0],bi=b(au),bk=[0,b(aw)],bl=b(az),bn=[0,b(aA)],bo=[0,b(ay)],bp=b(as),br=[0,b(at)],bs=b(R),bu=[0,b(s)],bv=b(au),bx=[0,b(aw)],by=b(az),bA=[0,b(aA)],bB=b(R),bD=[0,b(s)],bE=[0,[0,b(ay)],0],bF=b(as),bH=[0,b(at)],bI=b(R),bK=[0,b(s)],bL=b(R),bN=[0,b(s)],bO=b(s),bd=b(S),bb=b(S),a$=b(S),a9=b(S),a5=b(av),a6=b("cont"),a7=b("x"),bg=b(s),aB=e.Not_found,aD=e.Pp,aF=e.Environ,aC=e.Reductionops,aE=e.Hashtbl,a2=e.Tacentries,a3=e.Geninterp,a1=e.Tacenv,a4=e.Array;function
t(b,a){return i(af[4],aH,[0,aG,b],a)}var
D=[n,function(a){return t(aJ,aI)}],E=[n,function(a){return t(aL,aK)}],F=[n,function(a){return t(aN,aM)}],G=[n,function(a){return t(aP,aO)}],H=[n,function(a){return t(aR,aQ)}],I=[n,function(a){return t(aT,aS)}],M=a(f[20][1],[0,d[141]]);function
x(b){return a(ah[6],aU)}function
U(b){var
c=a(d[99],b);return a(d[B],c)}function
ai(c){var
b=a(h[1][7],c);return m.caml_int_of_string(i(f[15][4],b,1,m.caml_ml_string_length(b)-1|0))}function
aj(b){var
d=a(v[20],b),e=c(v[16],aV,d);return a(h[1][5],e)}function
N(j,i,b){var
e=a(d[B],j);if(11===e[0]){var
g=e[1],h=g[1];if(0===h[2]){var
k=g[2],l=h[1],m=function(c){return a(d[114],b-c|0)},n=c(f[19][2],b,m),o=[0,a(d[131],[0,[0,[0,l,0],i+1|0],k]),n];return a(d[q],o)}}return x(0)}function
ak(b,k){function
h(l){var
e=l;for(;;){var
b=a(d[B],e);switch(b[0]){case
5:var
e=b[1];continue;case
9:var
j=b[2],g=b[1];if(a(d[1],g))if(a(d[29],g)===k){var
o=a(f[19][38],j);return[11,[0,aj(a(d[29],o))]]}var
p=c(f[19][15],h,j),q=T[16],r=a(L[2],0);return[4,i(ad[7],r,q,g),p];default:var
m=T[16],n=a(L[2],0);return i(ad[7],n,m,e)}}}return h(b)}function
V(E,D,l){try{var
Y=a(d[41],E),m=Y}catch(a){a=$(a);if(a!==d[28])throw a;var
m=x(0)}var
G=a(L[2],0),o=U(c(aF[58],G,m));if(14===o[0]){var
p=o[1],q=p[1];if(1===q[1].length-1)if(0===q[2]){var
e=p[2];if(1===e[1].length-1)if(1===e[2].length-1){var
r=e[3];if(1===r.length-1){var
s=a(d[80],r[1]),b=s[1],H=s[2],I=a(f[17][1],b),J=a(K[62][5],l),O=a(u[48][4],l),P=i(aC[77],0,J,O),g=U(H);if(13===g[0]){var
t=g[2],h=[0,0],j=[0,0],k=[0,0],Q=g[4],R=function(i,t){var
o=a(d[80],t),e=o[2],g=a(f[17][1],o[1]);if(a(d[1],e))if(1===a(d[29],e)){k[1]=[0,N(a(f[17][3],b)[2],i,g)];return 0}var
p=a(d[39],e),q=p[2],u=p[1];if(q){var
r=q[2];if(r){var
l=r[2];if(l){var
m=l[2];if(m)if(!m[2]){var
y=m[1];if(a(d[1],l[1]))if(a(d[1],y)){var
s=z(F),B=A===s?F[1]:n===s?a(w[2],F):F;if(c(P,u,B)){j[1]=[0,N(a(f[17][3],b)[2],i,g)];return 0}}}}}}var
v=h[1],x=ak(e,(I+g|0)+1|0);h[1]=[0,[0,N(a(f[17][3],b)[2],i,g),x],v];return 0};c(f[19][14],R,Q);var
v=a(ae[3],k[1]),S=v?a(ae[3],j[1]):v;if(S)x(0);var
y=a(d[B],t),T=7===y[0]?a(C[56],y[3]):t,V=k[1],W=i(f[17][16],M[4],D,M[1]),X=j[1];return[0,a(f[17][6],h[1]),X,T,W,V]}return x(0)}}}}return x(0)}function
O(e,j){var
g=j;for(;;){var
h=c(M[3],g,e);if(h)return h;var
b=a(d[B],g);switch(b[0]){case
5:var
g=b[1];continue;case
9:var
k=b[2],i=O(e,b[1]);if(i){var
l=function(a){return O(e,a)};return c(f[19][30],l,k)}return i;default:return 0}}}function
al(e,c){var
h=e.length-1,b=z(E),l=h>>>1|0,i=A===b?E[1]:n===b?a(w[2],E):E,j=z(D),k=[0,c],m=A===j?D[1]:n===j?a(w[2],D):D,f=a(d[q],[0,m,k]);function
g(b){if(h<b)return f;if(l<b){var
j=b-1|0,m=[0,i,[0,c,aq(e,j)[j+1],f,f]];return a(d[q],m)}var
n=g((2*b|0)+1|0),k=b-1|0,o=g(2*b|0),p=[0,i,[0,c,aq(e,k)[k+1],o,n]];return a(d[q],p)}return g(1)}function
am(e){function
b(a){return 1===a?0:[0,1===(a%2|0)?1:0,b(a>>>1|0)]}var
c=z(I),g=A===c?I[1]:n===c?a(w[2],I):I,h=b(e),j=a(f[17][6],h);function
k(g,f){var
h=[0,f];if(g)var
b=z(G),i=A===b?G[1]:n===b?a(w[2],G):G,c=i;else
var
e=z(H),j=A===e?H[1]:n===e?a(w[2],H):H,c=j;return a(d[q],[0,c,h])}return i(f[17][16],k,j,g)}function
W(h,k,g){var
b=k;for(;;){var
j=i(u[36],h,b,g);if(j)return j;var
e=a(d[B],b);switch(e[0]){case
5:var
b=e[1];continue;case
9:var
l=e[2],m=function(a){return W(h,a,g)};return c(f[19][28],m,l);default:return 0}}}function
an(f,a){function
g(a,b){if(b){var
h=b[2],e=b[1];return c(d[ax],a,e)?b:W(f,a,e)?[0,a,[0,e,h]]:[0,e,g(a,h)]}return[0,a,0]}if(a){var
b=a[1];return g(b,an(f,a[2]))}return 0}var
P=a(aE[18],[0,d[ax],d[151]]);function
X(d,b){a(af[11],aW);var
j=a(P[1],17),g=[0,0],k=[0,1];function
l(b){function
e(r){var
e=r;for(;;){if(e){var
m=e[1],s=e[2],t=m[2],u=m[1];try{var
v=T[16],w=a(L[2],0),x=aa(ag[3],w,v,t,b),y=a(h[1][10][17],x),z=function(a){var
b=a[1],c=l(a[2]);return[0,ai(b),c]},A=c(f[17][12],z,y),B=c(C[55],A,u);return B}catch(a){a=$(a);if(a===ag[1]){var
e=s;continue}throw a}}var
n=d[2];if(n){var
o=d[5],D=n[1];if(o){var
E=o[1];if(O(d[4],b))return c(C[55],[0,[0,1,b],0],E)}try{var
G=c(P[7],j,b);return G}catch(a){a=$(a);if(a===aB){var
F=[0,[0,1,am(k[1])],0],p=c(C[55],F,D);k[1]++;g[1]=[0,b,g[1]];i(P[5],j,b,p);return p}throw a}}var
q=d[5];if(q)return c(C[55],[0,[0,1,b],0],q[1]);var
H=a(aD[1],aX);return i(ah[3],0,0,H)}}return e(d[1])}var
e=c(f[17][12],l,b),m=d[3],n=a(f[17][6],g[1]);return[0,e,al(a(f[19][12],n),m)]}function
aY(n,m){var
b=[0,function(b){var
e=c(u[48][2],n,b);function
o(a){return c(u[48][2],a,b)}var
h=V(e,c(f[17][12],o,m),b),k=X(h,[0,a(K[62][3],b),0]),g=k[1];if(g)if(!g[2]){var
l=g[1],p=k[2];if(h[2]){var
r=a(d[q],[0,e,[0,p,l]]);return i(ac[3],0,r,2)}var
s=a(d[q],[0,e,[0,l]]);return i(ac[3],0,s,2)}throw[0,j,aZ]}];return a(K[62][9],b)}var
y=[0,t,D,E,F,G,H,I,M,x,U,ai,aj,N,ak,V,O,al,am,W,an,P,X,aY,function(h,o,n,m){var
b=[0,function(b){var
e=c(u[48][2],n,b);function
p(a){return c(u[48][2],a,b)}var
i=V(e,c(f[17][12],p,m),b),k=X(i,[0,o,0]),g=k[1];if(g)if(!g[2]){var
l=g[1],r=k[2];return i[2]?a(h,a(d[q],[0,e,[0,r,l]])):a(h,a(d[q],[0,e,[0,l]]))}throw[0,j,a0]}];return a(K[62][9],b)}];ab(84,y,"Quote_plugin.Quote");a(ao[12],a5);var
J=o[4],Y=a(h[1][5],a6),Z=a(h[1][5],a7);function
_(d,b){var
e=a(k[2][1],b),f=a3[4][1],g=c(h[1][10][5],Z,e),j=[0,i(h[1][10][4],Y,d,g),f];return c(k[18],j,[29,[0,J,[3,J,[1,[0,J,Y]],[0,[2,[1,[0,J,Z]]],0]]]])}var
a8=0,a_=[0,function(b){if(b){var
d=b[2];if(d){var
e=d[2];if(e){var
f=e[2];if(f)if(!f[2]){var
h=f[1],i=e[1],j=d[1],m=b[1],n=a(l[6],g[4]),o=c(k[2][7],n,m),p=a(l[17],g[4]),q=a(l[6],p),r=c(k[2][7],q,j),s=a(l[6],g[8]),t=c(k[2][7],s,i),u=a(l[6],g[14]),w=c(k[2][7],u,h);return function(b){function
a(a){return _(w,a)}return aa(y[24],a,t,o,r)}}}}}return a(v[2],a9)},a8],ba=[0,function(b){if(b){var
d=b[2];if(d){var
e=d[2];if(e)if(!e[2]){var
f=e[1],h=d[1],i=b[1],j=a(l[6],g[4]),m=c(k[2][7],j,i),n=a(l[6],g[8]),o=c(k[2][7],n,h),p=a(l[6],g[14]),q=c(k[2][7],p,f);return function(c){var
a=0;function
b(a){return _(q,a)}return aa(y[24],b,o,m,a)}}}}return a(v[2],a$)},a_],bc=[0,function(b){if(b){var
d=b[2];if(d)if(!d[2]){var
e=d[1],f=b[1],h=a(l[6],g[4]),i=c(k[2][7],h,f),j=a(l[17],g[4]),m=a(l[6],j),n=c(k[2][7],m,e);return function(a){return c(y[23],i,n)}}}return a(v[2],bb)},ba],be=[0,function(b){if(b)if(!b[2]){var
d=b[1],e=a(l[6],g[4]),f=c(k[2][7],e,d);return function(a){return c(y[23],f,0)}}return a(v[2],bd)},bc],bf=a(a4[12],be);i(a1[9],0,[0,Q,bg],bf);function
bh(N){var
s=a(h[1][6],bi),b=g[14],q=0,r=0;if(0===b[0]){var
t=[0,bk,[0,[1,o[4],[5,[0,b[1]]],s],r]],u=a(h[1][6],bl),d=g[8];if(0===d[0]){var
v=[0,bo,[0,bn,[0,[1,o[4],[5,[0,d[1]]],u],t]]],w=a(h[1][6],bp),e=g[4];if(0===e[0]){var
x=[0,br,[0,[1,o[4],[0,[5,[0,e[1]]]],w],v]],y=a(h[1][6],bs),f=g[4];if(0===f[0]){var
z=[0,[0,bu,[0,[1,o[4],[5,[0,f[1]]],y],x]],q],B=a(h[1][6],bv),i=g[14],A=0;if(0===i[0]){var
C=[0,bx,[0,[1,o[4],[5,[0,i[1]]],B],A]],D=a(h[1][6],by),k=g[8];if(0===k[0]){var
E=[0,bA,[0,[1,o[4],[5,[0,k[1]]],D],C]],F=a(h[1][6],bB),l=g[4];if(0===l[0]){var
G=[0,[0,bD,[0,[1,o[4],[5,[0,l[1]]],F],E]],z],H=a(h[1][6],bF),m=g[4];if(0===m[0]){var
I=[0,bH,[0,[1,o[4],[0,[5,[0,m[1]]]],H],bE]],J=a(h[1][6],bI),n=g[4];if(0===n[0]){var
K=[0,[0,bK,[0,[1,o[4],[5,[0,n[1]]],J],I]],G],M=a(h[1][6],bL),p=g[4],L=0;if(0===p[0])return c(a2[4],[0,Q,bO],[0,[0,bN,[0,[1,o[4],[5,[0,p[1]]],M],L]],K]);throw[0,j,bM]}throw[0,j,bJ]}throw[0,j,bG]}throw[0,j,bC]}throw[0,j,bz]}throw[0,j,bw]}throw[0,j,bt]}throw[0,j,bq]}throw[0,j,bm]}throw[0,j,bj]}c(ao[19],bh,Q);var
ap=[0,Q,J,Y,Z,_];ab(94,ap,"Quote_plugin.G_quote");ab(95,[0,y,ap],"Quote_plugin");return});
