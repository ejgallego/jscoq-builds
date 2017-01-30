(function(bP){"use strict";var
B=140,p="plugins/quote/g_quote.ml4",aA="in",A=250,av="quote_plugin",q=124,au="$k",at="[",n=246,az="$c",S="Extension: cannot occur",s="quote",ay="]",as="$lc",r="Quote",R="$f",ar="plugins/quote/quote.ml",ax=136,aw="using",m=bP.jsoo_runtime,aq=m.caml_check_bound,b=m.caml_new_string,z=m.caml_obj_tag,ab=m.caml_register_global,$=m.caml_wrap_exception;function
a(a,b){return a.length==1?a(b):m.caml_call_gen(a,[b])}function
c(a,b,c){return a.length==2?a(b,c):m.caml_call_gen(a,[b,c])}function
i(a,b,c,d){return a.length==3?a(b,c,d):m.caml_call_gen(a,[b,c,d])}function
aa(a,b,c,d,e){return a.length==4?a(b,c,d,e):m.caml_call_gen(a,[b,c,d,e])}var
e=m.caml_get_global_data(),Q=b(av),d=e.Term,f=e.Util,w=e.Tacmach,j=e.Assert_failure,N=e.Proofview,an=e.Tactics,U=e.Evd,M=e.Global,am=e.Constr_matching,h=e.Names,I=e.Termops,ad=e.CErrors,ac=e.Coqlib,x=e.CamlinternalLazy,ai=e.Option,ah=e.Patternops,v=e.Pervasives,g=e.Constrarg,o=e.Loc,l=e.Genarg,k=e.Tacinterp,ao=e.Mltop,a0=[0,b(ar),473,11],aZ=[0,b(ar),458,13],aW=b("invalid inversion scheme for quote"),aU=[0,b("Coq"),[0,b(s),[0,b(r),0]]],aQ=b("M"),aP=b("Quote: not a simple fixpoint"),aN=b("End_idx"),aO=[0,b(r),0],aL=b("Left_idx"),aM=[0,b(r),0],aJ=b("Right_idx"),aK=[0,b(r),0],aH=b("varmap_find"),aI=[0,b(r),0],aF=b("Node_vm"),aG=[0,b(r),0],aD=b("Empty_vm"),aE=[0,b(r),0],aB=b(s),aC=b(r),bL=[0,b(p),1,0],bI=[0,b(p),1,0],bF=[0,b(p),1,0],bB=[0,b(p),1,0],by=[0,b(p),1,0],bv=[0,b(p),1,0],bs=[0,b(p),1,0],bp=[0,b(p),1,0],bl=[0,b(p),1,0],bi=[0,b(p),1,0],bh=b(au),bj=[0,b(aw)],bk=b(az),bm=[0,b(aA)],bn=[0,b(ay)],bo=b(as),bq=[0,b(at)],br=b(R),bt=[0,b(s)],bu=b(au),bw=[0,b(aw)],bx=b(az),bz=[0,b(aA)],bA=b(R),bC=[0,b(s)],bD=[0,[0,b(ay)],0],bE=b(as),bG=[0,b(at)],bH=b(R),bJ=[0,b(s)],bK=b(R),bM=[0,b(s)],bN=b(s),ba=b(S),a_=b(S),a8=b(S),a6=b(S),a1=b(av),a2=b("cont"),a3=b("x"),be=b(s),aV=e.Not_found,aX=e.Pp,aR=e.Environ,aS=e.Reductionops,aT=e.Hashtbl,bO=e.Tacentries,a4=e.Geninterp,bc=e.Array,bf=e.Tacenv;function
t(b,a){return i(ac[4],aC,[0,aB,b],a)}var
C=[n,function(a){return t(aE,aD)}],D=[n,function(a){return t(aG,aF)}],E=[n,function(a){return t(aI,aH)}],F=[n,function(a){return t(aK,aJ)}],G=[n,function(a){return t(aM,aL)}],H=[n,function(a){return t(aO,aN)}],K=a(f[20][1],[0,d[141]]);function
u(b){return a(ad[6],aP)}function
T(b){var
c=a(d[99],b);return a(d[B],c)}function
ae(c){var
b=a(h[1][7],c);return m.caml_int_of_string(i(f[15][4],b,1,m.caml_ml_string_length(b)-1|0))}function
af(b){var
d=a(v[20],b),e=c(v[16],aQ,d);return a(h[1][5],e)}function
L(j,i,b){var
e=a(d[B],j);if(11===e[0]){var
g=e[1],h=g[1];if(0===h[2]){var
k=g[2],l=h[1],m=function(c){return a(d[114],b-c|0)},n=c(f[19][2],b,m),o=[0,a(d[131],[0,[0,[0,l,0],i+1|0],k]),n];return a(d[q],o)}}return u(0)}function
ag(b,k){function
h(l){var
e=l;for(;;){var
b=a(d[B],e);switch(b[0]){case
5:var
e=b[1];continue;case
9:var
j=b[2],g=b[1];if(a(d[1],g))if(a(d[29],g)===k){var
o=a(f[19][38],j);return[11,[0,af(a(d[29],o))]]}var
p=c(f[19][15],h,j),q=U[16],r=a(M[2],0);return[4,i(ah[7],r,q,g),p];default:var
m=U[16],n=a(M[2],0);return i(ah[7],n,m,e)}}}return h(b)}function
V(D,C,l){try{var
Y=a(d[41],D),m=Y}catch(a){a=$(a);if(a!==d[28])throw a;var
m=u(0)}var
F=a(M[2],0),o=T(c(aR[58],F,m));if(14===o[0]){var
p=o[1],q=p[1];if(1===q[1].length-1)if(0===q[2]){var
e=p[2];if(1===e[1].length-1)if(1===e[2].length-1){var
r=e[3];if(1===r.length-1){var
s=a(d[80],r[1]),b=s[1],G=s[2],H=a(f[17][1],b),J=a(N[62][5],l),O=a(w[48][4],l),P=i(aS[77],0,J,O),g=T(G);if(13===g[0]){var
t=g[2],h=[0,0],j=[0,0],k=[0,0],Q=g[4],R=function(i,t){var
o=a(d[80],t),e=o[2],g=a(f[17][1],o[1]);if(a(d[1],e))if(1===a(d[29],e)){k[1]=[0,L(a(f[17][3],b)[2],i,g)];return 0}var
p=a(d[39],e),q=p[2],u=p[1];if(q){var
r=q[2];if(r){var
l=r[2];if(l){var
m=l[2];if(m)if(!m[2]){var
y=m[1];if(a(d[1],l[1]))if(a(d[1],y)){var
s=z(E),B=A===s?E[1]:n===s?a(x[2],E):E;if(c(P,u,B)){j[1]=[0,L(a(f[17][3],b)[2],i,g)];return 0}}}}}}var
v=h[1],w=ag(e,(H+g|0)+1|0);h[1]=[0,[0,L(a(f[17][3],b)[2],i,g),w],v];return 0};c(f[19][14],R,Q);var
v=a(ai[3],k[1]),S=v?a(ai[3],j[1]):v;if(S)u(0);var
y=a(d[B],t),U=7===y[0]?a(I[56],y[3]):t,V=k[1],W=i(f[17][16],K[4],C,K[1]),X=j[1];return[0,a(f[17][6],h[1]),X,U,W,V]}return u(0)}}}}return u(0)}function
O(e,j){var
g=j;for(;;){var
h=c(K[3],g,e);if(h)return h;var
b=a(d[B],g);switch(b[0]){case
5:var
g=b[1];continue;case
9:var
k=b[2],i=O(e,b[1]);if(i){var
l=function(a){return O(e,a)};return c(f[19][30],l,k)}return i;default:return 0}}}function
aj(e,c){var
h=e.length-1,b=z(D),l=h>>>1|0,i=A===b?D[1]:n===b?a(x[2],D):D,j=z(C),k=[0,c],m=A===j?C[1]:n===j?a(x[2],C):C,f=a(d[q],[0,m,k]);function
g(b){if(h<b)return f;if(l<b){var
j=b-1|0,m=[0,i,[0,c,aq(e,j)[j+1],f,f]];return a(d[q],m)}var
n=g((2*b|0)+1|0),k=b-1|0,o=g(2*b|0),p=[0,i,[0,c,aq(e,k)[k+1],o,n]];return a(d[q],p)}return g(1)}function
ak(e){function
b(a){return 1===a?0:[0,1===(a%2|0)?1:0,b(a>>>1|0)]}var
c=z(H),g=A===c?H[1]:n===c?a(x[2],H):H,h=b(e),j=a(f[17][6],h);function
k(g,f){var
h=[0,f];if(g)var
b=z(F),i=A===b?F[1]:n===b?a(x[2],F):F,c=i;else
var
e=z(G),j=A===e?G[1]:n===e?a(x[2],G):G,c=j;return a(d[q],[0,c,h])}return i(f[17][16],k,j,g)}function
W(h,k,g){var
b=k;for(;;){var
j=i(w[36],h,b,g);if(j)return j;var
e=a(d[B],b);switch(e[0]){case
5:var
b=e[1];continue;case
9:var
l=e[2],m=function(a){return W(h,a,g)};return c(f[19][28],m,l);default:return 0}}}function
al(f,a){function
g(a,b){if(b){var
h=b[2],e=b[1];return c(d[ax],a,e)?b:W(f,a,e)?[0,a,[0,e,h]]:[0,e,g(a,h)]}return[0,a,0]}if(a){var
b=a[1];return g(b,al(f,a[2]))}return 0}var
P=a(aT[18],[0,d[ax],d[151]]);function
X(d,b){a(ac[11],aU);var
j=a(P[1],17),g=[0,0],k=[0,1];function
l(b){function
e(r){var
e=r;for(;;){if(e){var
m=e[1],s=e[2],t=m[2],u=m[1];try{var
v=U[16],w=a(M[2],0),x=aa(am[3],w,v,t,b),y=a(h[1][10][17],x),z=function(a){var
b=a[1],c=l(a[2]);return[0,ae(b),c]},A=c(f[17][12],z,y),B=c(I[55],A,u);return B}catch(a){a=$(a);if(a===am[1]){var
e=s;continue}throw a}}var
n=d[2];if(n){var
o=d[5],C=n[1];if(o){var
D=o[1];if(O(d[4],b))return c(I[55],[0,[0,1,b],0],D)}try{var
F=c(P[7],j,b);return F}catch(a){a=$(a);if(a===aV){var
E=[0,[0,1,ak(k[1])],0],p=c(I[55],E,C);k[1]++;g[1]=[0,b,g[1]];i(P[5],j,b,p);return p}throw a}}var
q=d[5];if(q)return c(I[55],[0,[0,1,b],0],q[1]);var
G=a(aX[1],aW);return i(ad[3],0,0,G)}}return e(d[1])}var
e=c(f[17][12],l,b),m=d[3],n=a(f[17][6],g[1]);return[0,e,aj(a(f[19][12],n),m)]}function
aY(n,m){var
b=[0,function(b){var
e=c(w[48][2],n,b);function
o(a){return c(w[48][2],a,b)}var
h=V(e,c(f[17][12],o,m),b),k=X(h,[0,a(N[62][3],b),0]),g=k[1];if(g)if(!g[2]){var
l=g[1],p=k[2];if(h[2]){var
r=a(d[q],[0,e,[0,p,l]]);return i(an[3],0,r,2)}var
s=a(d[q],[0,e,[0,l]]);return i(an[3],0,s,2)}throw[0,j,aZ]}];return a(N[62][9],b)}var
y=[0,t,C,D,E,F,G,H,K,u,T,ae,af,L,ag,V,O,aj,ak,W,al,P,X,aY,function(h,o,n,m){var
b=[0,function(b){var
e=c(w[48][2],n,b);function
p(a){return c(w[48][2],a,b)}var
i=V(e,c(f[17][12],p,m),b),k=X(i,[0,o,0]),g=k[1];if(g)if(!g[2]){var
l=g[1],r=k[2];return i[2]?a(h,a(d[q],[0,e,[0,r,l]])):a(h,a(d[q],[0,e,[0,l]]))}throw[0,j,a0]}];return a(N[62][9],b)}];ab(84,y,"Quote_plugin.Quote");a(ao[12],a1);var
J=o[4],Y=a(h[1][5],a2),Z=a(h[1][5],a3);function
_(d,b){var
e=a(k[2][1],b),f=a4[4][1],g=c(h[1][10][5],Z,e),j=[0,i(h[1][10][4],Y,d,g),f];return c(k[18],j,[29,[0,J,[3,J,[1,[0,J,Y]],[0,[2,[1,[0,J,Z]]],0]]]])}var
a5=0,a7=[0,function(b){if(b){var
d=b[2];if(d){var
e=d[2];if(e){var
f=e[2];if(f)if(!f[2]){var
h=f[1],i=e[1],j=d[1],m=b[1],n=a(l[6],g[4]),o=c(k[2][7],n,m),p=a(l[17],g[4]),q=a(l[6],p),r=c(k[2][7],q,j),s=a(l[6],g[8]),t=c(k[2][7],s,i),u=a(l[6],g[14]),w=c(k[2][7],u,h);return function(b){function
a(a){return _(w,a)}return aa(y[24],a,t,o,r)}}}}}return a(v[2],a6)},a5],a9=[0,function(b){if(b){var
d=b[2];if(d){var
e=d[2];if(e)if(!e[2]){var
f=e[1],h=d[1],i=b[1],j=a(l[6],g[4]),m=c(k[2][7],j,i),n=a(l[6],g[8]),o=c(k[2][7],n,h),p=a(l[6],g[14]),q=c(k[2][7],p,f);return function(c){var
a=0;function
b(a){return _(q,a)}return aa(y[24],b,o,m,a)}}}}return a(v[2],a8)},a7],a$=[0,function(b){if(b){var
d=b[2];if(d)if(!d[2]){var
e=d[1],f=b[1],h=a(l[6],g[4]),i=c(k[2][7],h,f),j=a(l[17],g[4]),m=a(l[6],j),n=c(k[2][7],m,e);return function(a){return c(y[23],i,n)}}}return a(v[2],a_)},a9],bb=[0,function(b){if(b)if(!b[2]){var
d=b[1],e=a(l[6],g[4]),f=c(k[2][7],e,d);return function(a){return c(y[23],f,0)}}return a(v[2],ba)},a$],bd=a(bc[12],bb);i(bf[9],0,[0,Q,be],bd);function
bg(N){var
s=a(h[1][6],bh),b=g[14],q=0,r=0;if(0===b[0]){var
t=[0,bj,[0,[1,o[4],[5,[0,b[1]]],s],r]],u=a(h[1][6],bk),d=g[8];if(0===d[0]){var
v=[0,bn,[0,bm,[0,[1,o[4],[5,[0,d[1]]],u],t]]],w=a(h[1][6],bo),e=g[4];if(0===e[0]){var
x=[0,bq,[0,[1,o[4],[0,[5,[0,e[1]]]],w],v]],y=a(h[1][6],br),f=g[4];if(0===f[0]){var
z=[0,[0,bt,[0,[1,o[4],[5,[0,f[1]]],y],x]],q],B=a(h[1][6],bu),i=g[14],A=0;if(0===i[0]){var
C=[0,bw,[0,[1,o[4],[5,[0,i[1]]],B],A]],D=a(h[1][6],bx),k=g[8];if(0===k[0]){var
E=[0,bz,[0,[1,o[4],[5,[0,k[1]]],D],C]],F=a(h[1][6],bA),l=g[4];if(0===l[0]){var
G=[0,[0,bC,[0,[1,o[4],[5,[0,l[1]]],F],E]],z],H=a(h[1][6],bE),m=g[4];if(0===m[0]){var
I=[0,bG,[0,[1,o[4],[0,[5,[0,m[1]]]],H],bD]],J=a(h[1][6],bH),n=g[4];if(0===n[0]){var
K=[0,[0,bJ,[0,[1,o[4],[5,[0,n[1]]],J],I]],G],M=a(h[1][6],bK),p=g[4],L=0;if(0===p[0])return c(bO[4],[0,Q,bN],[0,[0,bM,[0,[1,o[4],[5,[0,p[1]]],M],L]],K]);throw[0,j,bL]}throw[0,j,bI]}throw[0,j,bF]}throw[0,j,bB]}throw[0,j,by]}throw[0,j,bv]}throw[0,j,bs]}throw[0,j,bp]}throw[0,j,bl]}throw[0,j,bi]}c(ao[19],bg,Q);var
ap=[0,Q,J,Y,Z,_];ab(94,ap,"Quote_plugin.G_quote");ab(95,[0,y,ap],"Quote_plugin");return});
