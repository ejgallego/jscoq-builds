function(k_){"use strict";var
bf=104,cF=",",be="AddSetoidRing",M='"',cE="field_mod",u=250,cD="(",a$="AddSetoidField",cC="constants",aO="Field_tac",cl="with carrier ",cA="postprocess tactic",cB="Init",aN="field",cz="InitialRing",o=246,ck="decidable",cw="Pphi_pow",cx='Using setoid "',cy="tactic recognizing constants",Y="Extension: cannot occur",cv="postprocess",bd="gen_phiZ",aQ="setoid",cu="ring_mod",cj="$map",C="]",ci="preprocess",a_="power_tac",ct="Print",bb="closed_term",bc="power",cs="Ring_polynom",ba="protect_fv",an="Ring",ch='and "',cg="$lH",ce="morphism",cf="plugins/setoid_ring/newring.ml",cr="field_lookup",ab="Coq",cc="closed",cd="F",aP="x",cb="ring_lookup",a9="ring",ca="preprocess tactic",a8=127,B="[",cq='and morphisms "',b$="and equivalence relation ",b_=")",b7="field_mods",b8=":",b9="t",b5="$f",b6="abstract",aM="sign",b4="PEeval",aL="div",a7="newring_plugin",co="Add",cp="completeness",a6="IDphi",cn=101,cm="ring_mods",b3="Pphi_dev",H=k_.jsoo_runtime,F=H.caml_check_bound,b=H.caml_new_string,t=H.caml_obj_tag,aK=H.caml_register_global,E=H.caml_wrap_exception;function
a(a,b){return a.length==1?a(b):H.caml_call_gen(a,[b])}function
c(a,b,c){return a.length==2?a(b,c):H.caml_call_gen(a,[b,c])}function
e(a,b,c,d){return a.length==3?a(b,c,d):H.caml_call_gen(a,[b,c,d])}function
L(a,b,c,d,e){return a.length==4?a(b,c,d,e):H.caml_call_gen(a,[b,c,d,e])}function
Q(a,b,c,d,e,f){return a.length==5?a(b,c,d,e,f):H.caml_call_gen(a,[b,c,d,e,f])}function
k9(a,b,c,d,e,f,g,h){return a.length==7?a(b,c,d,e,f,g,h):H.caml_call_gen(a,[b,c,d,e,f,g,h])}var
f=H.caml_get_global_data(),bt=[0,b(a7),b("get_res")],bv=[0,[0,b(ab),[0,b("Setoids"),[0,b("Setoid"),0]]],[0,[0,b(ab),[0,b("Lists"),[0,b("List"),0]]],[0,[0,b(ab),[0,b(cB),[0,b("Datatypes"),0]]],[0,[0,b(ab),[0,b(cB),[0,b("Logic"),0]]],0]]]],at=b("setoid_ring"),af=b(a7),S=f.Constr,G=f.CClosure,j=f.Util,k=f.EConstr,bT=f.Tacmach,v=f.Proofview,g=f.Ltac_plugin,R=f.Coqlib,_=f.Global,i=f.Names,q=f.Pervasives,bP=f.Lib,s=f.CamlinternalLazy,aq=f.Universes,I=f.Not_found,z=f.Mod_subst,P=f.Libnames,aa=f.Retyping,bB=f.Reductionops,d=f.Pp,Z=f.CErrors,D=f.Printer,bC=f.Assert_failure,ao=f.Termops,aC=f.Typing,T=f.Evarutil,bJ=f.Smartlocate,l=f.Loc,ae=f.Feedback,bF=f.Flags,aV=f.CAst,p=f.Stdarg,h=f.Genarg,ar=f.Evd,bp=f.Univops,bq=f.Declare,bo=f.Constrintern,aU=f.Quote_plugin,bl=f.Tactics,bj=f.Globnames,az=f.Summary,aA=f.Libobject,bZ=f.Egramml,ai=f.Vernac_classifier,bY=f.Vernacinterp,bX=f.Pfedit,K=f.Ppconstr,n=f.Pcoq,m=f.CLexer,ah=f.CList,bg=[0],eT=f.Option,c9=f.Refiner,c1=f.Environ,c2=f.Goal,cU=f.DAst,cR=f.Tacticals,cN=f.Esubst,fS=f.Redexpr,gK=f.Mltop;aK(288,bg,"Newring_plugin.Newring_ast");var
gI=[0,0,0],gJ=[0,0,0],gx=b("field kind"),gy=b(cy),gz=b(ca),gA=b(cA),gB=b(aQ),gC=b(bc),gD=b(aM),gE=b(aL),gF=b("infinite property"),gn=[0,b(aO),0],go=[0,0,0],gp=b("field_lemmas"),gq=b("_field_lemma1"),gr=b("_field_lemma2"),gs=b("_field_lemma3"),gt=b("_field_lemma4"),gu=b("_lemma5"),gw=[22,0],gv=[22,0],gm=b("field inverse should be declared as a morphism"),f9=b("arguments of field_simplify do not have all the same type"),f_=[0,b(aN)],f8=[0,b(aO),0],f$=b(M),ga=b(M),gb=b("cannot find a declared field structure over"),gc=[0,b(aN)],gd=[0,b(cf),866,12],f3=[0,1],f4=[0,0],f2=b("bad field structure"),e5=[0,0,0],e6=[0,0,0],eV=b("ring kind"),eW=b(cy),eX=b(ca),eY=b(cA),eZ=b(aQ),e0=b(bc),e1=b(aM),e2=b(aL),eU=b(" cannot be set twice"),eN=[0,b("Ring_base"),0],eO=b("ring_lemmas"),eP=b("_ring_lemma1"),eQ=b("_ring_lemma2"),eS=[22,0],eR=[22,0],eL=[0,1],eM=[0,0],eK=b("bad ring structure"),et=b("ring addition should be declared as a morphism"),eu=b("ring multiplication should be declared as a morphism"),ev=b("ring opposite should be declared as a morphism"),ew=b(M),ex=b(ch),ey=b(M),ez=b(M),eA=b('",'),eB=b(cq),eC=b(M),eD=b(cx),eE=b(M),eF=b(ch),eG=b(M),eH=b(cq),eI=b(M),eJ=b(cx),ep=b("cannot find setoid relation"),eb=b("arguments of ring_simplify do not have all the same type"),ec=[0,b(a9)],ed=b(M),ee=b(M),ef=b("cannot find a declared ring structure over"),eg=[0,b(a9)],eh=[0,b(cf),373,12],du=[0,b(cs),0],dt=b("newring"),dn=b(an),dm=b(an),di=b("ring: cannot find relation (not closed)"),dh=b("ring: cannot find relation"),c$=b(an),c_=b(an),c8=b(aP),c7=b(aP),c5=b(aP),c3=b("Ring.exec_tactic: anomaly"),cY=b(aP),cZ=b(cd),c0=b(cd),cW=[2,1],cX=[0,1],cS=[0,b(a7),b(bb)],cT=b(b9),cV=b(b9),cK=b("not found"),cL=b("map "),cM=[0,b("lookup_map")],cI=b("dummy"),cH=b("global_head_of_constr."),da=b("Build_Setoid_Theory"),dc=b("None"),dd=b("Some"),de=b("eq"),df=b("cons"),dg=b("nil"),dj=b(ab),dk=[0,[0,b("Ring_theory"),0],[0,[0,b(cs),0],[0,[0,b("Ring_tac"),0],[0,[0,b(cz),0],[0,[0,b(aO),0],[0,[0,b("Field_theory"),0],0]]]]]],dp=[0,b(ab),0],dq=b(cz),dv=b("almost_ring_theory"),dw=b("Eqsth"),dy=b("Eq_ext"),dA=b("Eq_s_ext"),dC=b("ring_theory"),dD=b("mk_reqe"),dF=b("semi_ring_theory"),dG=b("mk_seqe"),dI=b("Abstract"),dJ=b("Computational"),dL=b("Morphism"),dN=b("inv_morph_nothing"),dO=b("mkhypo"),dQ=b("hypo"),dT=b(b4),dW=b(cw),dZ=b(b3),d2=b(bd),d5=b(a6),d9=b(a9),d_=b("ring-tac-carrier-table"),d$=b("ring-tac-name-table"),ej=b("tactic-new-ring-theory"),e8=[0,b(ab),0],e9=b(aO),fc=b("FEeval"),ff=b(b4),fi=b(cw),fl=b(b3),fo=b("display_pow_linear"),fr=b("display_linear"),fu=b(bd),fx=b(a6),fB=b(aN),fE=b("PCond"),fH=b(bd),fK=b(a6),fO=b("field_cond"),fP=b(aN),fR=b("simpl_field_expr"),fT=b("almost_field_theory"),fU=b("field_theory"),fV=b("semi_field_theory"),fW=b("AF_AR"),fY=b("F_R"),f0=b("SF_SR"),f5=b("field-tac-carrier-table"),f6=b("field-tac-name-table"),gf=b("tactic-new-field-theory"),kQ=b(a$),kA=b(a$),kx=b(Y),kv=b(Y),kt=b(a$),kq=b(Y),kn=b(b$),ko=b(cl),km=b("The following field structures have been declared:"),kl=b(Y),jX=b(cp),jE=b(be),jo=b(be),jl=b(Y),jj=b(Y),jh=b(be),je=b(Y),jb=b(b$),jc=b(cl),ja=b("The following ring structures have been declared:"),i$=b(Y),hO=[0,0],hg=b(b6),hh=b(ck),hi=b(ce),hj=b(C),hk=b(B),hl=b(cC),hm=b(C),hn=b(B),ho=b(cc),hp=b(C),hq=b(B),hr=b(ci),hs=b(C),ht=b(B),hu=b(cv),hv=b(aQ),hw=b(C),hx=b(B),hy=b(a_),hz=b(C),hA=b(B),hB=b(a_),hC=b(aM),hD=b(aL),gN=b(cj),gQ=b(ba),gT=b("$id"),gW=b("in"),gY=b(cj),g1=b(ba),g3=b(ba),g6=[0,b(C),0],g7=b("$l"),g_=b(B),ha=b("$t"),hd=b(bb),hf=b(bb),hE=b(cu),hG=b(cu),hL=b(ck),hP=b(b6),hT=b(ce),hW=b(C),hZ=b(B),h1=b(cC),h4=b(C),h7=b(B),h9=b(cc),ia=b(C),id=b(B),ig=b(ci),ij=b(C),im=b(B),ip=b(cv),iu=b(aQ),iy=b(aM),iB=b(C),iE=b(B),iH=b(bc),iK=b(C),iN=b(B),iQ=b(a_),iU=b(aL),iX=b(cm),iZ=b(cm),i3=b(b_),i5=b(cF),i7=b(cD),jp=[0,[0,[0,b(ct)],[0,[0,b("Rings")],0]],0],jw=[0,b(b8)],jA=[0,b(an)],jB=[0,b(co)],jH=b("$lrt"),jK=b(C),jM=b(cg),jP=b(B),jR=b(b5),jU=b(cb),jW=b(cb),jY=b(cE),j0=b(cE),j6=b(cp),j9=b(b7),j$=b(b7),kd=b(b_),kf=b(cF),kh=b(cD),kB=[0,[0,[0,b(ct)],[0,[0,b("Fields")],0]],0],kI=[0,b(b8)],kM=[0,b("Field")],kN=[0,b(co)],kT=b("$lt"),kW=b(C),kY=b(cg),k1=b(B),k3=b(b5),k6=b(cr),k8=b(cr);function
N(b){var
c=a(d[3],b);return e(Z[6],0,0,c)}function
bh(g,f,e,d,b){switch(a(f,d)){case
0:return c(G[56],e,b);case
1:return a(G[39],b);default:return-1===d?c(G[56],e,b):a(g,b)}}function
cG(b,f){var
g=c(k[82],b,f)[1];try{var
i=c(ao[103],b,g)[1];return i}catch(b){b=E(b);if(b===I){var
h=a(d[3],cH);return e(Z[3],0,0,h)}throw b}}function
bi(b){try{var
c=a(bj[16],b);return c}catch(b){b=E(b);if(b===I)return[0,a(i[1][6],cI)];throw b}}function
ap(f,d,b){var
k=a(f,bi(b));if(k){var
p=k[1],q=-1;return bh(function(a){return ap(f,d,a)},p,d,q,b)}var
h=a(S[26],b);switch(h[0]){case
6:var
r=function(a,b){return ap(f,a,b)};return e(G[58],r,d,b);case
9:var
g=h[2],i=h[1],s=0;if(g.length-1<=0){var
t=a(S[13],[0,i,g]);return a(G[39],t)}var
l=c(j[19][51],s,g),u=l[2],m=a(S[13],[0,i,l[1]]),n=a(f,bi(m));if(n){var
v=n[1],o=function(b,a){return bh(function(a){return ap(f,d,a)},v,d,b,a)},w=c(j[19][16],o,u),x=[6,o(-1,m),w];return a(G[40],x)}var
y=a(S[13],[0,i,g]);return a(G[39],y);default:return a(G[39],b)}}function
bk(b,a){try{var
c=[0,e(j[17][133],bj[5],a,b)];return c}catch(a){a=E(a);if(a===I)return 0;throw a}}var
aR=[0,j[15][49][1]];function
aS(b,a){aR[1]=e(j[15][49][4],b,a,aR[1]);return 0}function
cJ(f){try{var
b=c(j[15][49][22],f,aR[1]);return b}catch(b){b=E(b);if(b===I){var
g=a(d[3],cK),h=a(d[20],f),i=a(d[3],cL),k=c(d[12],i,h),l=c(d[12],k,g);return e(Z[6],0,cM,l)}throw b}}function
aT(g,f,d,b){function
h(a){return c(T[17],d,a)}var
i=a(k[a8][1],b),j=a(cN[1],0),l=ap(c(cJ(g),d,b),j,i),m=a(G[23],0),n=e(G[44],[0,h],G[9],f),o=e(G[61],n,m,l);return a(k[8],o)}function
cO(a){var
b=0,c=2,d=[0,function(b,c,d){return aT(a,b,c,d)},c];return e(bl[49],0,d,b)}function
cP(b,a){var
c=[0,[0,a,0]],d=2,f=[0,function(a,c,d){return aT(b,a,c,d)},d];return e(bl[49],0,f,c)}function
cQ(f,b){function
g(g){var
h=c(j[17][15],aq[50],b),i=e(j[17][19],aU[1][8][4],h,aU[1][8][1]);if(e(aU[1][16],g,i,f))return a(v[16],0);var
k=a(d[7],0);return c(cR[66][4],0,k)}return c(v[71][1],v[54],g)}function
bm(b){var
d=[0,cS,0];function
e(a){return[0,c(l[11],0,a)]}var
f=c(j[17][15],e,b),g=a(h[18],p[10]),k=a(h[5],g),m=[0,[0,c(h[7],k,f)],0],n=[1,a(i[1][6],cT)],o=[0,c(cU[3],0,n),0],q=a(h[5],p[13]),r=[0,d,[0,[0,c(h[7],q,o)],m]],s=[31,c(l[11],0,r)];return[28,[0,[0,[0,a(i[1][6],cV)],0],s]]}function
bn(d){var
b=a(_[2],0),f=a(ar[17],b),c=e(bo[13],b,f,d);return[0,c[1],c[2]]}function
O(c){var
b=a(_[2],0),d=a(ar[17],b);return L(bo[10],b,d,0,c)[1]}function
$(e,d,b){var
f=a(_[2],0),g=c(bp[1],f,b),h=[0,[0,c(bp[2],d,g)]],j=[0,[0,k9(bq[2],0,cX,0,0,h,0,b)],cW],k=a(i[1][6],e),l=Q(bq[3],0,0,k,0,j);return a(S[15],l)}function
br(n,m){function
o(g,b){var
d=b[1],h=b[3],j=b[2],k=a(q[21],d),l=c(q[16],cY,k),f=a(i[1][6],l),m=[2,[1,c(aV[1],0,f)]];return[0,d+1|0,[0,m,j],e(i[1][11][4],f,g,h)]}var
b=e(j[17][19],o,m,[0,0,0,i[1][11][1]]),p=b[3],r=b[2],s=a(i[1][6],cZ),t=e(i[1][11][4],s,n,p),u=[0,t,a(g[13][31],0)[2]],d=a(i[1][6],c0),f=[0,[1,c(aV[1],0,d)],r],h=[3,c(l[11],0,f)],k=[29,c(l[11],0,h)];return c(g[13][23],u,k)}var
bs=[0,[0]],c4=[0,bt,0],c6=[0,function(d,b){var
e=a(j[17][5],d),f=a(h[6],p[3]),k=c(g[13][2][7],f,e);function
l(d){var
e=b[1],f=a(q[21],d),g=c(q[16],c5,f),h=a(i[1][6],g);return c(i[1][11][22],h,e)}bs[1]=c(j[19][2],k,l);return a(v[16],0)}];e(g[4][15],0,bt,c6);function
bu(G,F,m,b,E){function
H(h,b){var
d=b[1],j=b[3],k=b[2],l=a(q[21],d),m=c(q[16],c7,l),f=a(i[1][6],m),n=[2,[1,c(aV[1],0,f)]],o=a(g[13][2][1],h);return[0,d+1|0,[0,n,k],e(i[1][11][4],f,o,j)]}var
n=e(j[17][19],H,E,[0,0,0,i[1][11][1]]),I=n[3],J=n[2],K=[0,I,a(g[13][31],0)[2]];function
M(b){var
d=a(q[21],b),e=c(q[16],c8,d);return a(i[1][6],e)}var
N=c(j[17][54],m,M),O=a(h[5],p[3]),P=[0,c4,[0,[0,c(h[7],O,m)],0]],Q=[31,c(l[11],0,P)];function
R(a){return[0,a]}var
S=[5,[28,[0,c(j[17][15],R,N),Q]]],A=ar[3][1],B=k[14],C=a(c1[10],G),f=L(c2[3][6],F,C,B,A),D=[0,f[1],f[3]],d=t(b),U=c(j[18],J,[0,S,0]),w=u===d?b[1]:o===d?a(s[2],b):b,x=[0,[0,c(l[11],0,w)],U],y=[3,c(l[11],0,x)],z=[29,c(l[11],0,y)],V=c(g[13][23],K,z),W=c(v[70][8],V,D),X=a(c9[2],W),r=a(T[47],X),Y=r[2],Z=r[1];function
_(c){var
b=a(g[13][2][2],c),d=b?a(k[a8][1],b[1]):a(q[2],c3);return a(Y,d)}var
$=a(ar[148],Z);return[0,c(j[19][15],_,bs[1]),$]}function
bw(b){return[o,function(f){var
c=e(R[4],c_,bv,b),d=a(aq[50],c);return a(k[8],d)}]}function
as(a){return[o,function(b){return e(R[4],c$,bv,a)}]}var
db=bw(da),aW=as(dc),aX=as(dd),U=bw(de),ak=as(df),al=as(dg);function
am(b,d){var
c=t(b),e=u===c?b[1]:o===c?a(s[2],b):b;return a(k[21],[0,e,d])}function
w(f,b,e){var
d=t(b),g=u===d?b[1]:o===d?a(s[2],b):b,h=[0,c(T[14],f,g),e];return a(k[21],h)}function
bx(f,l){var
d=c(k[3],f,l);if(9===d[0]){var
b=d[2],m=d[1];if(2<=b.length-1){var
n=[0,m,e(j[19][7],b,0,b.length-1-2|0)],g=a(k[21],n);if(c(k[107][16],f,g)){var
h=b.length-1-1|0,i=b.length-1-2|0,o=F(b,h)[h+1];return[0,g,F(b,i)[i+1],o]}return N(di)}}return N(dh)}var
au=[0,dj,[0,at,0]];function
dl(a){return c(j[18],au,a)}var
by=c(j[17][15],dl,dk);function
V(b){return[o,function(f){var
c=e(R[4],dm,by,b),d=a(aq[50],c);return a(k[8],d)}]}function
r(a){return[o,function(b){return e(R[4],dn,by,a)}]}var
dr=c(j[17][15],i[1][6],[0,dq,[0,at,dp]]),ds=a(i[5][4],dr);function
bz(b){return[o,function(d){var
c=a(i[6][4],b);return e(i[13][1],[0,ds],i[5][6],c)}]}function
ac(a){var
b=[0,at,du];return[o,function(c){return e(R[2],dt,b,a)}]}var
av=V(dv),dx=r(dw),dz=r(dy),dB=r(dA),aw=V(dC),dE=V(dD),ax=V(dF),dH=V(dG),ay=V(dI),dK=V(dJ),dM=V(dL),W=bz(dN),dP=r(dO),y=r(dQ);function
bA(g,d,f){var
b=f;for(;;){var
e=c(k[3],d,b);if(6===e[0]){var
b=e[3];continue}var
h=bx(d,b)[1],i=function(c){var
b=c[1],d=t(b),e=c[2],f=u===d?b[1]:o===d?a(s[2],b):b;return[0,f,e]},l=c(j[17][15],i,g),m=function(a){return-1===a?1:2},n=[0,[0,cG(d,h),m],l];return function(a){return bk(n,a)}}}var
dR=0;function
dS(b){var
a=b+1|0;if(!(14<a>>>0))switch(a){case
9:case
13:return 2;case
0:case
11:case
14:return 0}return 1}var
dU=[0,[0,ac(dT),dS],dR];function
dV(b){var
a=b+1|0;if(!(18<a>>>0))switch(a){case
12:case
17:return 2;case
0:case
9:case
10:case
11:case
14:case
16:case
18:return 0}return 1}var
dX=[0,[0,ac(dW),dV],dU];function
dY(a){if(11<=a)if(14<=a)var
b=15<=a?0:1;else{if(12!==a)return 2;var
b=1}else
var
b=-1===a?1:8<=a?1:0;return b?0:1}var
d0=[0,[0,ac(dZ),dY],dX];function
d1(a){return 0}var
d3=[0,[0,r(d2),d1],d0];function
d4(a){return 0}var
d6=[0,[0,r(d5),d4],d3],d7=[0,[0,al,function(a){return-1===a?0:1}],d6],d8=[0,[0,ak,function(a){return-1===a?0:2===a?2:1}],d7];aS(d9,function(a,b){return bA(d8,a,b)});var
ad=a(j[21][1],[0,S[80]]),aY=e(az[4],0,d_,ad[1]),aZ=e(az[4],0,d$,P[24][1]);function
ea(a){return c(ad[22],a,aY[1])}function
ei(d){var
a=d[2],b=d[1],e=c(z[45],b,a[1]),f=c(z[45],b,a[2]),h=c(z[45],b,a[3]),i=c(z[45],b,a[4]),j=c(z[45],b,a[5]),k=c(z[45],b,a[6]),l=c(z[45],b,a[9]),m=c(z[45],b,a[10]),n=c(g[3][1],b,a[7]),o=c(g[3][1],b,a[8]),p=c(g[3][1],b,a[11]),q=c(g[3][1],b,a[12]);if(e===a[1])if(f===a[2])if(c(S[74],h,a[3]))if(i===a[4])if(j===a[5])if(k===a[6])if(l===a[9])if(m===a[10])if(n===a[7])if(o===a[8])if(p===a[11])if(q===a[12])return a;return[0,e,f,h,i,j,k,n,o,l,m,p,q]}function
bD(b){var
a=b[2],c=b[1][1];aY[1]=e(ad[4],a[1],a,aY[1]);aZ[1]=e(P[24][4],c,a,aZ[1]);return 0}var
aB=a(aA[1],ej),ek=aB[8],el=aB[7];function
em(a){return[0,a]}function
en(c,b){var
a=1===c?1:0;return a?bD(b):a}var
eo=a(aA[4],[0,aB[1],bD,aB[3],en,em,ei,el,ek]);function
bE(c,d,b,a){try{var
e=L(g[23][11],c,d[1],b,a),i=e[2],f=L(g[23][12],c,e[1],b,a),j=f[2],h=L(g[23][13],c,f[1],b,a),k=h[2];d[1]=h[1];var
l=am(db,[0,b,a,i,j,k]);return l}catch(a){a=E(a);if(a===I)return N(ep);throw a}}function
eq(h,g,f,e,d,c,b,a){return am(dE,[0,h,g,f,e,d,c,b,a])}function
er(f,e,d,c,b,a){return am(dH,[0,f,e,d,c,b,a])}function
es(i,b,j){var
f=j[5],n=j[4],l=j[3],m=j[2],h=j[1],r=c(k[3],b[1],f);if(9===r[0])if(1===r[2].length-1){var
F=t(U),aR=r[1],aS=u===F?U[1]:o===F?a(s[2],U):U;if(e(k[95],b[1],aR,aS)){var
aT=w(b,dx,[0,h]),aU=n?w(b,dz,[0,h,m,l,n[1]]):w(b,dB,[0,h,m,l]),aV=e(aC[7],i,b,aT);return[0,aV,e(aC[7],i,b,aU)]}}var
v=[0,[0,[0,[0,h,[0,f]]],[0,[0,[0,h,[0,f]]],0]],[0,[0,h,[0,f]]]],G=bE(a(_[2],0),b,h,f);try{var
aQ=c(g[23][14],v,m),x=aQ}catch(a){a=E(a);if(a!==I)throw a;var
x=N(et)}var
p=x[2];try{var
aP=c(g[23][14],v,l),y=aP}catch(a){a=E(a);if(a!==I)throw a;var
y=N(eu)}var
q=y[2];if(n){var
z=n[1];try{var
aq=c(g[23][14],[0,[0,[0,[0,h,[0,f]]],0],[0,[0,h,[0,f]]]],z),A=aq}catch(a){a=E(a);if(a!==I)throw a;var
A=N(ev)}var
B=A[2],H=eq(h,m,l,z,f,p,q,B),J=a(d[3],ew),K=e(D[15],i,b[1],B),L=a(d[3],ex),M=a(d[13],0),O=a(d[3],ey),P=e(D[15],i,b[1],q),Q=a(d[3],ez),R=a(d[13],0),S=a(d[3],eA),T=e(D[15],i,b[1],p),V=a(d[3],eB),W=a(d[13],0),X=a(d[3],eC),Y=e(D[15],i,b[1],f),Z=a(d[3],eD),$=c(d[12],Z,Y),aa=c(d[12],$,X),ab=c(d[12],aa,W),ac=c(d[12],ab,V),ad=c(d[12],ac,T),af=c(d[12],ad,S),ag=c(d[12],af,R),ah=c(d[12],ag,Q),ai=c(d[12],ah,P),aj=c(d[12],ai,O),ak=c(d[12],aj,M),al=c(d[12],ak,L),am=c(d[12],al,K),an=c(d[12],am,J),ao=ae[6],ap=function(a){return c(ao,0,a)};c(bF[25],ap,an);var
C=H}else{var
ar=a(d[3],eE),as=e(D[15],i,b[1],q),at=a(d[3],eF),au=a(d[13],0),av=a(d[3],eG),aw=e(D[15],i,b[1],p),ax=a(d[3],eH),ay=a(d[13],0),az=a(d[3],eI),aA=e(D[15],i,b[1],f),aB=a(d[3],eJ),aD=c(d[12],aB,aA),aE=c(d[12],aD,az),aF=c(d[12],aE,ay),aG=c(d[12],aF,ax),aH=c(d[12],aG,aw),aI=c(d[12],aH,av),aJ=c(d[12],aI,au),aK=c(d[12],aJ,at),aL=c(d[12],aK,as),aM=c(d[12],aL,ar),aN=ae[6],aO=function(a){return c(aN,0,a)};c(bF[25],aO,aM);var
C=er(h,m,l,f,p,q)}return[0,G,C]}function
bG(h,g,f,e,d,c,b,a){return a?a[1]:es(h,g,[0,f,e,d,c,b])}function
bH(b){if(typeof
b==="number"){var
c=t(ay);return u===c?ay[1]:o===c?a(s[2],ay):ay}else
return 0===b[0]?am(dK,[0,b[1]]):am(dM,[0,b[1]])}function
bI(w,v,r,q,p,d){if(d){var
b=d[1];if(0===b[0])return a(g[9][3],b[1]);var
f=b[1],h=bJ[3],i=function(a){return c(h,0,a)};return bm(c(j[17][15],i,f))}var
e=t(W),k=u===e?W[1]:o===e?a(s[2],W):W,m=[0,[0,c(l[11],0,k)],0],n=[3,c(l[11],0,m)];return[29,c(l[11],0,n)]}function
aD(c,b,a){return w(b,dP,[0,Q(aa[2],0,0,c,b[1],a),a])}function
bK(d,b,h){var
f=t(y),i=u===f?y[1]:o===f?a(s[2],y):y,g=c(T[14],b,i),l=w(b,al,[0,g]);function
m(c,a){return w(b,ak,[0,g,aD(d,b,c),a])}var
n=e(j[17][19],m,h,l),p=e(aC[7],d,b,n),q=a(k[a8][1],p);return c(T[46],b[1],q)}function
bL(n,b,e){var
f=t(y),p=u===f?y[1]:o===f?a(s[2],y):y,h=c(T[14],b,p);if(e){var
i=e[1],d=i[1],q=i[2];if(0===d[0])var
k=a(g[9][3],d[1]);else
var
r=d[1],v=bJ[3],x=function(a){return c(v,0,a)},k=bm(c(j[17][15],x,r));return[0,k,w(b,aX,[0,h,aD(n,b,O(q))])]}var
m=t(W),z=u===m?W[1]:o===m?a(s[2],W):W,A=[0,c(l[11],0,z)],B=w(b,aW,[0,h]),C=[3,c(l[11],0,[0,A,0])];return[0,[29,c(l[11],0,C)],B]}function
bM(g,b,d){var
e=t(y),h=u===e?y[1]:o===e?a(s[2],y):y,f=c(T[14],b,h);return d?w(b,aX,[0,f,aD(g,b,O(d[1]))]):w(b,aW,[0,f])}function
bN(g,b,d){var
e=t(y),h=u===e?y[1]:o===e?a(s[2],y):y,f=c(T[14],b,h);return d?w(b,aX,[0,f,aD(g,b,O(d[1]))]):w(b,aW,[0,f])}function
bO(v,L,ar,K,aq,J,ap,ao,an){var
M=J[2],O=J[1],P=L[2],f=L[1],as=c(j[18],au,eN);a(R[3],as);var
l=a(_[2],0),ad=Q(aa[2],0,0,l,f,P),p=c(k[3],f,ad);if(9===p[0]){var
b=p[2],w=b.length-1-6|0;if(2<w>>>0)var
h=0;else{var
r=p[1];switch(w){case
0:var
x=t(ax),ae=b[1],af=b[2],ag=b[3],ah=b[4],ai=b[5],aj=b[6],ak=u===x?ax[1]:o===x?a(s[2],ax):ax;if(e(k[95],f,r,ak))var
d=[0,eL,ae,af,ag,ah,ai,0,0,aj],h=1;else
var
h=0;break;case
1:var
h=0;break;default:var
y=b[1],z=b[2],A=b[3],B=b[4],C=b[5],D=b[6],E=b[7],G=b[8],H=t(av),al=u===H?av[1]:o===H?a(s[2],av):av;if(e(k[95],f,r,al))var
d=[0,0,y,z,A,B,C,[0,D],[0,E],G],h=1;else{var
I=t(aw),am=u===I?aw[1]:o===I?a(s[2],aw):aw;if(e(k[95],f,r,am))var
d=[0,eM,y,z,A,B,C,[0,D],[0,E],G],h=1;else
var
h=0}}}}else
var
h=0;if(!h)var
d=N(eK);var
S=d[9],T=d[8],U=d[6],V=d[5],W=d[2],m=[0,f],at=d[4],ay=d[3],az=d[1],X=bG(l,m,W,V,U,T,S,ar),Y=X[1],aA=X[2],Z=bL(l,m,ap),aB=Z[2],aC=Z[1],aD=bM(l,m,ao),aE=bN(l,m,an),aF=[0,Y,[0,aA,[0,P,[0,aB,[0,aD,[0,aE,[0,bH(K),0]]]]]]],aG=bz(eO),ab=bu(l,m[1],5,aG,aF),ac=ab[2],n=ab[1],aH=F(n,3)[4],aI=F(n,4)[5],aJ=a(i[1][8],v),aK=$(c(q[16],aJ,eP),ac,aH),aL=a(i[1][8],v),aM=$(c(q[16],aL,eQ),ac,aI),aN=bI(l,f,K,az,[0,ay,at,V,U,T],aq),aO=O?a(g[9][3],O[1]):eS,aP=M?a(g[9][3],M[1]):eR,aQ=c(k[5],f,W),aR=c(k[5],f,S),aS=c(k[5],f,Y),aT=F(n,0)[1],aU=F(n,2)[3],aV=a(eo,[0,aQ,aR,aS,F(n,1)[2],aU,aT,aN,aC,aK,aM,aO,aP]);c(bP[6],v,aV);return 0}function
bQ(a){return typeof
a==="number"?0:0===a[0]?[0,O(a[1])]:[1,O(a[1])]}function
x(e,b,d){return a(eT[3],b[1])?(b[1]=[0,d],0):N(c(q[16],e,eU))}function
e3(q,p,o){var
l=bn(p),b=[0,0],d=[0,0],e=[0,0],f=[0,0],g=[0,0],h=[0,0],i=[0,0],k=[0,0],r=l[2],s=l[1];function
m(a){switch(a[0]){case
0:return x(eV,b,bQ(a[1]));case
1:return x(eW,e,a[1]);case
2:return x(eX,f,a[1]);case
3:return x(eY,g,a[1]);case
4:var
c=a[1],j=O(a[2]);return x(eZ,d,[0,O(c),j]);case
5:return x(e0,i,[0,a[1],a[2]]);case
6:return x(e1,h,a[1]);default:return x(e2,k,a[1])}}c(j[17][14],m,o);var
a=b[1],n=a?a[1]:0;return bO(q,[0,s,r],d[1],n,e[1],[0,f[1],g[1]],i[1],h[1],k[1])}function
bR(d,a,c){if(a)return a;var
b=bx(d,c);return[0,b[2],[0,b[3],0]]}function
bS(d,a,b,c){var
f=w(a,al,[0,b]);function
g(d,c){return w(a,ak,[0,b,d,c])}var
h=e(j[17][19],g,c,f);return e(aC[7],d,a,h)}function
A(b){var
c=a(k[8],b);return a(g[13][2][1],c)}function
X(b){var
d=a(g[13][31],0);return c(g[13][2][6],d,b)}function
e4(a){var
b=A(a[2]),c=A(a[3]),d=A(a[4]),e=A(a[5]),f=A(a[6]),g=X(a[7]),h=X(a[8]),i=A(a[9]),j=A(a[10]),k=X([28,[0,e5,a[11]]]);return[0,b,[0,c,[0,d,[0,e,[0,f,[0,g,[0,h,[0,i,[0,j,[0,k,[0,X([28,[0,e6,a[12]]]),0]]]]]]]]]]]}function
e7(H,G,F,C){function
b(n){var
b=a(bT[42][4],n),f=a(v[66][5],n);try{var
h=bR(b,F,C),l=[0,b];if(h){var
o=h[2],i=Q(aa[2],0,0,f,b,h[1]),p=function(g){var
h=Q(aa[2],0,0,f,b,g),c=1-Q(bB[79],0,f,b,i,h);if(c){var
j=a(d[3],eb);return e(Z[6],0,ec,j)}return c};c(j[17][14],p,o);try{var
B=ea(c(k[5],b,i)),m=B}catch(g){g=E(g);if(g!==I)throw g;var
q=a(d[3],ed),r=e(D[15],f,b,i),s=a(d[3],ee),t=a(d[13],0),u=a(d[3],ef),w=c(d[12],u,t),x=c(d[12],w,s),y=c(d[12],x,r),z=c(d[12],y,q),m=e(Z[6],0,eg,z)}var
J=bS(f,l,a(k[8],m[1]),h),K=a(g[13][2][1],J),L=A(bK(f,l,G)),M=e4(m),N=br(H,c(j[18],M,[0,L,[0,K,0]])),O=a(v[64][1],l[1]),P=c(v[18],O,N);return P}throw[0,bC,eh]}catch(b){b=E(b);if(a(v[70][10],b))return c(v[21],0,b);throw b}}return a(v[66][10],b)}var
e_=c(j[17][15],i[1][6],[0,e9,[0,at,e8]]),e$=a(i[5][4],e_),fa=0;function
fb(b){var
a=b+1|0;if(!(16<a>>>0))switch(a){case
11:case
15:return 2;case
0:case
13:case
16:return 0}return 1}var
fd=[0,[0,r(fc),fb],fa];function
fe(b){var
a=b+1|0;if(!(14<a>>>0))switch(a){case
9:case
13:return 2;case
0:case
11:case
14:return 0}return 1}var
fg=[0,[0,ac(ff),fe],fd];function
fh(b){var
a=b+1|0;if(!(18<a>>>0))switch(a){case
12:case
17:return 2;case
0:case
9:case
10:case
11:case
14:case
16:case
18:return 0}return 1}var
fj=[0,[0,ac(fi),fh],fg];function
fk(a){if(11<=a)if(14<=a)var
b=15<=a?0:1;else{if(12!==a)return 2;var
b=1}else
var
b=-1===a?1:8<=a?1:0;return b?0:1}var
fm=[0,[0,ac(fl),fk],fj];function
fn(b){var
a=b+1|0;if(!(20<a>>>0))switch(a){case
13:case
18:return 2;case
0:case
10:case
11:case
12:case
15:case
17:case
19:case
20:return 0}return 1}var
fp=[0,[0,r(fo),fn],fm];function
fq(a){if(12<=a)if(15<=a)var
b=17<=a?0:1;else{if(13!==a)return 2;var
b=1}else
var
b=-1===a?1:9<=a?1:0;return b?0:1}var
fs=[0,[0,r(fr),fq],fp];function
ft(a){return 0}var
fv=[0,[0,r(fu),ft],fs];function
fw(a){return 0}var
fy=[0,[0,r(fx),fw],fv],fz=[0,[0,al,function(a){return-1===a?0:1}],fy],fA=[0,[0,ak,function(a){return-1===a?0:2===a?2:1}],fz];aS(fB,function(a,b){return bA(fA,a,b)});var
fC=0;function
fD(b){var
a=b+1|0;if(!(15<a>>>0))switch(a){case
10:case
14:return 2;case
0:case
12:case
15:return 0}return 1}var
fF=[0,[0,r(fE),fD],fC];function
fG(a){return 0}var
fI=[0,[0,r(fH),fG],fF];function
fJ(a){return 0}var
fL=[0,[0,r(fK),fJ],fI],fM=[0,[0,al,function(a){return-1===a?0:1}],fL],fN=[0,[0,ak,function(a){return-1===a?0:2===a?2:1}],fM];aS(fO,function(e,f){function
b(c){var
b=c[1],d=t(b),e=c[2],f=u===d?b[1]:o===d?a(s[2],b):b;return[0,f,e]}var
d=c(j[17][15],b,fN);return function(a){return bk(d,a)}});function
fQ(a,b,c){return aT(fP,a,b,c)}c(fS[3],fR,fQ);var
aE=r(fT),aF=r(fU),aG=r(fV),fX=r(fW),fZ=r(fY),f1=r(f0),a0=e(az[4],0,f5,ad[1]),a1=e(az[4],0,f6,P[24][1]);function
f7(a){return c(ad[22],a,a0[1])}function
ge(d){var
a=d[2],b=d[1],e=c(z[45],b,a[1]),f=c(z[45],b,a[2]),h=c(z[45],b,a[5]),i=c(z[45],b,a[6]),j=c(z[45],b,a[7]),k=c(z[45],b,a[8]),l=c(z[45],b,a[9]),m=c(g[3][1],b,a[3]),n=c(g[3][1],b,a[4]),o=c(g[3][1],b,a[10]),p=c(g[3][1],b,a[11]);if(e===a[1])if(f===a[2])if(h===a[5])if(i===a[6])if(j===a[7])if(k===a[8])if(l===a[9])if(m===a[3])if(n===a[4])if(o===a[10])if(p===a[11])return a;return[0,e,f,m,n,h,i,j,k,l,o,p]}function
bU(b){var
a=b[2],c=b[1][1];a0[1]=e(ad[4],a[1],a,a0[1]);a1[1]=e(P[24][4],c,a,a1[1]);return 0}var
aH=a(aA[1],gf),gg=aH[8],gh=aH[7];function
gi(a){return[0,a]}function
gj(c,b){var
a=1===c?1:0;return a?bU(b):a}var
gk=a(aA[4],[0,aH[1],bU,aH[3],gj,gi,ge,gh,gg]);function
gl(f,b,i,d){var
h=c(k[3],f[1],d);if(9===h[0])if(1===h[2].length-1){var
l=t(U),p=h[1],q=u===l?U[1]:o===l?a(s[2],U):U;if(e(k[95],f[1],p,q)){var
r=a(R[36],0)[6],v=a(aq[50],r),w=[0,a(k[8],v),[0,b,b,i]];return a(k[21],w)}}bE(a(_[2],0),f,b,d);var
m=[0,[0,[0,[0,b,[0,d]]],0],[0,[0,b,[0,d]]]];try{var
n=c(g[23][14],m,i),j=n}catch(a){a=E(a);if(a!==I)throw a;var
j=N(gm)}return j[2]}function
gG(m,br,bq){var
ar=[0,0],as=[0,0],at=[0,0],av=[0,0],aw=[0,0],ax=[0,0],ay=[0,0],az=[0,0],aA=[0,0];function
bo(b){if(0===b[0]){var
a=b[1];switch(a[0]){case
0:return x(gx,ar,bQ(a[1]));case
1:return x(gy,at,a[1]);case
2:return x(gz,av,a[1]);case
3:return x(gA,aw,a[1]);case
4:var
c=a[1],d=O(a[2]);return x(gB,as,[0,O(c),d]);case
5:return x(gC,az,[0,a[1],a[2]]);case
6:return x(gD,ay,a[1]);default:return x(gE,aA,a[1])}}return x(gF,ax,O(b[1]))}c(j[17][14],bo,bq);var
aB=ar[1],P=aB?aB[1]:0,aC=aA[1],aD=ay[1],aH=az[1],aI=aw[1],aJ=av[1],aK=at[1],aL=ax[1],bp=as[1],aR=c(j[18],au,gn);a(R[3],aR);var
ag=bn(br),p=ag[2],r=ag[1],l=a(_[2],0),d=[0,r],aN=Q(aa[2],0,0,l,d[1],p),J=c(k[3],d[1],aN);if(9===J[0]){var
b=J[2],T=b.length-1-8|0;if(2<T>>>0)var
h=0;else{var
K=J[1];switch(T){case
0:var
U=b[1],V=b[2],W=b[3],X=b[4],Y=b[5],Z=b[6],ab=b[7],ac=b[8],ad=t(aG),aO=u===ad?aG[1]:o===ad?a(s[2],aG):aG;if(e(ao[bf],d[1],aO,K))var
f=[0,f3,U,V,W,X,Y,0,0,Z,ab,ac,w(d,f1,[0,U,V,W,X,Y,Z,ab,ac,p])],h=1;else
var
h=0;break;case
1:var
h=0;break;default:var
y=b[1],z=b[2],A=b[3],B=b[4],C=b[5],D=b[6],E=b[7],G=b[8],H=b[9],I=b[10],ae=t(aE),aP=u===ae?aE[1]:o===ae?a(s[2],aE):aE;if(e(ao[bf],d[1],aP,K))var
f=[0,0,y,z,A,B,C,[0,D],[0,E],G,H,I,w(d,fX,[0,y,z,A,B,C,D,E,G,H,I,p])],h=1;else{var
af=t(aF),aQ=u===af?aF[1]:o===af?a(s[2],aF):aF;if(e(ao[bf],d[1],aQ,K))var
f=[0,f4,y,z,A,B,C,[0,D],[0,E],G,H,I,w(d,fZ,[0,y,z,A,B,C,D,E,G,H,I,p])],h=1;else
var
h=0}}}}else
var
h=0;if(!h)var
f=N(f2);var
L=f[11],ah=f[8],ai=f[6],aj=f[5],M=f[2],aS=f[12],aT=f[10],aU=f[4],aV=f[3],aW=f[1],ak=bG(l,d,M,aj,ai,ah,L,bp),al=ak[2],am=ak[1];bO(m,[0,d[1],aS],[0,[0,am,al]],P,aK,go,aH,aD,aC);var
an=bL(l,d,aH),aX=an[2],aY=an[1],aZ=bM(l,d,aD),a0=bN(l,d,aC),a1=gl(d,M,aT,L),a2=[0,am,[0,al,[0,a1,[0,p,[0,aX,[0,aZ,[0,a0,[0,bH(P),0]]]]]]]],aM=[o,function(c){var
b=a(i[6][4],gp);return e(i[13][1],[0,e$],i[5][6],b)}],ap=bu(l,d[1],9,aM,a2),v=ap[2],n=ap[1],a3=F(n,3)[4],a4=F(n,4)[5],a5=F(n,5)[6],a6=F(n,6)[7];if(aL)var
a7=[0,c(k[5],r,aL[1])],a8=[0,F(n,8)[9],a7],aq=a(S[13],a8);else
var
aq=F(n,7)[8];var
a9=a(i[1][8],m),a_=$(c(q[16],a9,gq),v,a3),a$=a(i[1][8],m),ba=$(c(q[16],a$,gr),v,a4),bb=a(i[1][8],m),bc=$(c(q[16],bb,gs),v,a5),bd=a(i[1][8],m),be=$(c(q[16],bd,gt),v,a6),bg=a(i[1][8],m),bh=$(c(q[16],bg,gu),v,aq),bi=bI(l,r,P,aW,[0,aV,aU,aj,ai,ah],aK),bj=aJ?a(g[9][3],aJ[1]):gw,bk=aI?a(g[9][3],aI[1]):gv,bl=c(k[5],r,M),bm=a(gk,[0,bl,c(k[5],r,L),bi,aY,a_,ba,bc,be,bh,bj,bk]);c(bP[6],m,bm);return 0}function
gH(a){var
b=A(a[2]),c=X(a[3]),d=X(a[4]),e=A(a[5]),f=A(a[7]),g=A(a[6]),h=A(a[8]),i=A(a[9]),j=X([28,[0,gI,a[10]]]);return[0,b,[0,c,[0,d,[0,e,[0,f,[0,g,[0,h,[0,i,[0,j,[0,X([28,[0,gJ,a[11]]]),0]]]]]]]]]]}var
J=[0,cP,cO,cQ,e3,aZ,e7,gG,a1,function(J,H,G,F){function
b(n){var
b=a(bT[42][4],n),f=a(v[66][5],n);try{var
h=bR(b,G,F),l=[0,b],o=c(j[18],au,f8);a(R[3],o);if(h){var
p=h[2],i=Q(aa[2],0,0,f,b,h[1]),q=function(g){var
h=Q(aa[2],0,0,f,b,g),c=1-Q(bB[79],0,f,b,i,h);if(c){var
j=a(d[3],f9);return e(Z[6],0,f_,j)}return c};c(j[17][14],q,p);try{var
C=f7(c(k[5],b,i)),m=C}catch(g){g=E(g);if(g!==I)throw g;var
r=a(d[3],f$),s=e(D[15],f,b,i),t=a(d[3],ga),u=a(d[13],0),w=a(d[3],gb),x=c(d[12],w,u),y=c(d[12],x,t),z=c(d[12],y,s),B=c(d[12],z,r),m=e(Z[6],0,gc,B)}var
K=bS(f,l,a(k[8],m[1]),h),L=a(g[13][2][1],K),M=A(bK(f,l,H)),N=gH(m),O=br(J,c(j[18],N,[0,M,[0,L,0]])),P=a(v[64][1],l[1]),S=c(v[18],P,O);return S}throw[0,bC,gd]}catch(b){b=E(b);if(a(v[70][10],b))return c(v[21],0,b);throw b}}return a(v[66][10],b)}];aK(339,J,"Newring_plugin.Newring");a(gK[10],af);var
gL=0;function
gM(b,c){return a(J[2],b)}var
gO=a(i[1][7],gN),gP=[0,[5,a(h[16],p[4])],gO],gR=[0,[0,[0,gQ,[1,c(l[11],0,gP),0]],gM],gL];function
gS(b,a,d){return c(J[1],b,a)}var
gU=a(i[1][7],gT),gV=[0,[5,a(h[16],p[8])],gU],gX=[0,gW,[1,c(l[11],0,gV),0]],gZ=a(i[1][7],gY),g0=[0,[5,a(h[16],p[4])],gZ],g2=[0,[0,[0,g1,[1,c(l[11],0,g0),gX]],gS],gR];L(g[10][8],af,g3,0,g2);var
g4=0;function
g5(b,a,d){return c(J[3],b,a)}var
g8=a(i[1][7],g7),g9=[0,[0,[5,a(h[16],p[23])]],g8],g$=[0,g_,[1,c(l[11],0,g9),g6]],hb=a(i[1][7],ha),hc=[0,[5,a(h[16],p[13])],hb],he=[0,[0,[0,hd,[1,c(l[11],0,hc),g$]],g5],g4];L(g[10][8],af,hf,0,he);function
aI(b){switch(b[0]){case
0:var
f=b[1];if(typeof
f==="number")return a(d[3],hg);else{if(0===f[0]){var
j=c(d[32],K[20],f[1]),k=a(d[3],hh);return c(d[12],k,j)}var
l=c(d[32],K[20],f[1]),m=a(d[3],hi);return c(d[12],m,l)}case
1:var
h=b[1];if(0===h[0]){var
n=h[1],o=a(d[3],hj),p=a(g[5][23],n),q=a(d[3],hk),r=a(d[13],0),s=a(d[3],hl),t=c(d[12],s,r),u=c(d[12],t,q),v=c(d[12],u,p);return c(d[12],v,o)}var
w=h[1],x=a(d[3],hm),y=e(d[39],d[13],P[41],w),z=a(d[3],hn),A=a(d[13],0),B=a(d[3],ho),C=c(d[12],B,A),D=c(d[12],C,z),E=c(d[12],D,y);return c(d[12],E,x);case
2:var
F=b[1],G=a(d[3],hp),H=a(g[5][23],F),I=a(d[3],hq),J=a(d[13],0),L=a(d[3],hr),M=c(d[12],L,J),N=c(d[12],M,I),O=c(d[12],N,H);return c(d[12],O,G);case
3:var
Q=b[1],R=a(d[3],hs),S=a(g[5][23],Q),T=a(d[3],ht),U=a(d[13],0),V=a(d[3],hu),W=c(d[12],V,U),X=c(d[12],W,T),Y=c(d[12],X,S);return c(d[12],Y,R);case
4:var
Z=b[1],_=c(d[32],K[20],b[2]),$=c(d[32],K[20],Z),aa=a(d[3],hv),ab=c(d[12],aa,$);return c(d[12],ab,_);case
5:var
i=b[1];if(0===i[0]){var
ac=b[2],ad=i[1],ae=a(d[3],hw),af=a(g[5][23],ad),ag=a(d[3],hx),ah=a(d[13],0),ai=c(d[32],K[20],ac),aj=a(d[3],hy),ak=c(d[12],aj,ai),al=c(d[12],ak,ah),am=c(d[12],al,ag),an=c(d[12],am,af);return c(d[12],an,ae)}var
ao=b[2],ap=i[1],aq=a(d[3],hz),ar=e(d[39],d[13],P[41],ap),as=a(d[3],hA),at=a(d[13],0),au=c(d[32],K[20],ao),av=a(d[3],hB),aw=c(d[12],av,au),ax=c(d[12],aw,at),ay=c(d[12],ax,as),az=c(d[12],ay,ar);return c(d[12],az,aq);case
6:var
aA=c(d[32],K[20],b[1]),aB=a(d[3],hC);return c(d[12],aB,aA);default:var
aC=c(d[32],K[20],b[1]),aD=a(d[3],hD);return c(d[12],aD,aC)}}var
a2=a(h[3],hE),hF=a(h[4],a2),aJ=e(n[13],n[9],hG,hF),hH=0,hI=0;function
hJ(a,c,b){return[0,[0,a]]}var
hK=[6,n[15][1]],hM=[0,[0,[0,[0,0,[0,a(m[10],hL)]],hK],hJ],hI];function
hN(b,a){return hO}var
hQ=[0,[0,[0,0,[0,a(m[10],hP)]],hN],hM];function
hR(a,c,b){return[0,[1,a]]}var
hS=[6,n[15][1]],hU=[0,[0,[0,[0,0,[0,a(m[10],hT)]],hS],hR],hQ];function
hV(e,a,d,c,b){return[1,[0,a]]}var
hX=[0,a(m[10],hW)],hY=[6,g[6][18]],h0=[0,a(m[10],hZ)],h2=[0,[0,[0,[0,[0,[0,0,[0,a(m[10],h1)]],h0],hY],hX],hV],hU];function
h3(e,a,d,c,b){return[1,[1,a]]}var
h5=[0,a(m[10],h4)],h6=[1,[6,n[15][7]]],h8=[0,a(m[10],h7)],h_=[0,[0,[0,[0,[0,[0,0,[0,a(m[10],h9)]],h8],h6],h5],h3],h2];function
h$(e,a,d,c,b){return[2,a]}var
ib=[0,a(m[10],ia)],ic=[6,g[6][18]],ie=[0,a(m[10],id)],ih=[0,[0,[0,[0,[0,[0,0,[0,a(m[10],ig)]],ie],ic],ib],h$],h_];function
ii(e,a,d,c,b){return[3,a]}var
ik=[0,a(m[10],ij)],il=[6,g[6][18]],io=[0,a(m[10],im)],iq=[0,[0,[0,[0,[0,[0,0,[0,a(m[10],ip)]],io],il],ik],ii],ih];function
ir(b,a,d,c){return[4,a,b]}var
is=[6,n[15][1]],it=[6,n[15][1]],iv=[0,[0,[0,[0,[0,0,[0,a(m[10],iu)]],it],is],ir],iq];function
iw(a,c,b){return[6,a]}var
ix=[6,n[15][1]],iz=[0,[0,[0,[0,0,[0,a(m[10],iy)]],ix],iw],iv];function
iA(f,b,e,a,d,c){return[5,[1,b],a]}var
iC=[0,a(m[10],iB)],iD=[1,[6,n[15][7]]],iF=[0,a(m[10],iE)],iG=[6,n[15][1]],iI=[0,[0,[0,[0,[0,[0,[0,0,[0,a(m[10],iH)]],iG],iF],iD],iC],iA],iz];function
iJ(f,b,e,a,d,c){return[5,[0,b],a]}var
iL=[0,a(m[10],iK)],iM=[6,g[6][18]],iO=[0,a(m[10],iN)],iP=[6,n[15][1]],iR=[0,[0,[0,[0,[0,[0,[0,0,[0,a(m[10],iQ)]],iP],iO],iM],iL],iJ],iI];function
iS(a,c,b){return[7,a]}var
iT=[6,n[15][1]],iV=[0,0,[0,[0,0,0,[0,[0,[0,[0,0,[0,a(m[10],iU)]],iT],iS],iR]],hH]];e(n[22],aJ,0,iV);function
iW(c,b,a){return aI}c(g[5][3],a2,iW);function
bV(b){var
c=e(d[39],d[28],aI,b);return a(d[46],c)}var
ag=a(h[3],iX),iY=a(h[4],ag),bW=e(n[13],n[9],iZ,iY),i0=0,i1=0;function
i2(d,a,c,b){return a}var
i4=[0,a(m[10],i3)],i6=[2,[6,aJ],[0,a(m[10],i5)]],i8=[0,0,[0,[0,0,0,[0,[0,[0,[0,[0,0,[0,a(m[10],i7)]],i6],i4],i2],i1]],i0]];e(n[22],bW,0,i8);function
i9(c,b,a){return bV}c(g[5][3],ag,i9);var
i_=0,jd=[0,[0,0,function(b){return b?a(q[2],i$):function(i,b){var
f=a(d[22],ja);c(ae[7],0,f);var
g=J[5][1];function
h(i,b){var
f=a(bX[6],0),g=f[2],h=f[1],j=e(D[7],g,h,b[2]),k=a(d[3],jb),l=a(d[13],0),m=e(D[7],g,h,b[1]),n=a(d[3],jc),o=a(d[13],0),p=a(P[20],i),q=a(K[9],p),r=c(d[12],q,o),s=c(d[12],r,n),t=c(d[12],s,m),u=c(d[12],t,l),v=c(d[12],u,k),w=c(d[12],v,j),x=c(d[26],2,w);return c(ae[7],0,x)}c(P[24][10],h,g);return b}}],i_],jf=[0,[0,0,function(b){if(b){var
d=b[2];if(d){var
f=d[2];if(f)if(!f[2]){var
i=f[1],j=d[1],k=b[1],l=a(h[4],p[8]),m=c(h[8],l,k),n=a(h[4],p[13]),o=c(h[8],n,j),r=a(h[19],ag),s=a(h[4],r),g=c(h[8],s,i);return function(c,a){var
b=g?g[1]:0;e(J[4],m,o,b);return a}}}}return a(q[2],je)}],jd];function
jg(b,a){return e(bY[2],a[1],[0,jh,b],a[2])}c(ah[87],jg,jf);var
ji=0,jk=[0,function(b){return b?a(q[2],jj):function(a){return ai[4]}},ji],jm=[0,function(b){if(b){var
c=b[2];if(c){var
d=c[2];if(d)if(!d[2])return function(a){return ai[5]}}}return a(q[2],jl)},jk];function
jn(b,a){return c(ai[3],[0,jo,b],a)}c(ah[87],jn,jm);var
jq=[5,[6,a(n[12],ag)]],jr=a(h[19],ag),js=[0,[0,a(h[4],jr)],jq],jt=[0,[1,c(l[11],0,js)],0],ju=[6,a(n[12],p[13])],jv=[0,[0,a(h[4],p[13])],ju],jx=[0,jw,[0,[1,c(l[11],0,jv)],jt]],jy=[6,a(n[12],p[8])],jz=[0,[0,a(h[4],p[8])],jy],jC=[0,[0,jB,[0,jA,[0,[1,c(l[11],0,jz)],jx]]],jp];function
jD(b,a){return e(bZ[1],[0,jE,b],0,a)}c(ah[87],jD,jC);var
jF=0;function
jG(e,d,c,f){var
b=a(j[17][cn],c);return L(J[6],e,d,b[2],b[1])}var
jI=a(i[1][7],jH),jJ=[0,[0,[5,a(h[16],p[13])]],jI],jL=[0,jK,[1,c(l[11],0,jJ),0]],jN=a(i[1][7],jM),jO=[0,[2,[5,a(h[16],p[13])]],jN],jQ=[0,jP,[1,c(l[11],0,jO),jL]],jS=a(i[1][7],jR),jT=[0,[6,a(h[16],g[2][1]),0],jS],jV=[0,[0,[0,jU,[1,c(l[11],0,jT),jQ]],jG],jF];L(g[10][8],af,jW,0,jV);function
a3(b){if(0===b[0])return aI(b[1]);var
e=c(d[32],K[20],b[1]),f=a(d[3],jX);return c(d[12],f,e)}var
a4=a(h[3],jY),jZ=a(h[4],a4),a5=e(n[13],n[9],j0,jZ),j1=0,j2=0,j3=[0,[0,[0,0,[6,aJ]],function(a,b){return[0,a]}],j2];function
j4(a,c,b){return[1,a]}var
j5=[6,n[15][1]],j7=[0,0,[0,[0,0,0,[0,[0,[0,[0,0,[0,a(m[10],j6)]],j5],j4],j3]],j1]];e(n[22],a5,0,j7);function
j8(c,b,a){return a3}c(g[5][3],a4,j8);function
b0(b){var
c=e(d[39],d[28],a3,b);return a(d[46],c)}var
aj=a(h[3],j9),j_=a(h[4],aj),b1=e(n[13],n[9],j$,j_),ka=0,kb=0;function
kc(d,a,c,b){return a}var
ke=[0,a(m[10],kd)],kg=[2,[6,a5],[0,a(m[10],kf)]],ki=[0,0,[0,[0,0,0,[0,[0,[0,[0,[0,0,[0,a(m[10],kh)]],kg],ke],kc],kb]],ka]];e(n[22],b1,0,ki);function
kj(c,b,a){return b0}c(g[5][3],aj,kj);var
kk=0,kp=[0,[0,0,function(b){return b?a(q[2],kl):function(i,b){var
f=a(d[22],km);c(ae[7],0,f);var
g=J[8][1];function
h(i,b){var
f=a(bX[6],0),g=f[2],h=f[1],j=e(D[7],g,h,b[2]),k=a(d[3],kn),l=a(d[13],0),m=e(D[7],g,h,b[1]),n=a(d[3],ko),o=a(d[13],0),p=a(P[20],i),q=a(K[9],p),r=c(d[12],q,o),s=c(d[12],r,n),t=c(d[12],s,m),u=c(d[12],t,l),v=c(d[12],u,k),w=c(d[12],v,j),x=c(d[26],2,w);return c(ae[7],0,x)}c(P[24][10],h,g);return b}}],kk],kr=[0,[0,0,function(b){if(b){var
d=b[2];if(d){var
f=d[2];if(f)if(!f[2]){var
i=f[1],j=d[1],k=b[1],l=a(h[4],p[8]),m=c(h[8],l,k),n=a(h[4],p[13]),o=c(h[8],n,j),r=a(h[19],aj),s=a(h[4],r),g=c(h[8],s,i);return function(c,a){var
b=g?g[1]:0;e(J[7],m,o,b);return a}}}}return a(q[2],kq)}],kp];function
ks(b,a){return e(bY[2],a[1],[0,kt,b],a[2])}c(ah[87],ks,kr);var
ku=0,kw=[0,function(b){return b?a(q[2],kv):function(a){return ai[4]}},ku],ky=[0,function(b){if(b){var
c=b[2];if(c){var
d=c[2];if(d)if(!d[2])return function(a){return ai[5]}}}return a(q[2],kx)},kw];function
kz(b,a){return c(ai[3],[0,kA,b],a)}c(ah[87],kz,ky);var
kC=[5,[6,a(n[12],aj)]],kD=a(h[19],aj),kE=[0,[0,a(h[4],kD)],kC],kF=[0,[1,c(l[11],0,kE)],0],kG=[6,a(n[12],p[13])],kH=[0,[0,a(h[4],p[13])],kG],kJ=[0,kI,[0,[1,c(l[11],0,kH)],kF]],kK=[6,a(n[12],p[8])],kL=[0,[0,a(h[4],p[8])],kK],kO=[0,[0,kN,[0,kM,[0,[1,c(l[11],0,kL)],kJ]]],kB];function
kP(b,a){return e(bZ[1],[0,kQ,b],0,a)}c(ah[87],kP,kO);var
kR=0;function
kS(e,d,c,f){var
b=a(j[17][cn],c);return L(J[9],e,d,b[2],b[1])}var
kU=a(i[1][7],kT),kV=[0,[0,[5,a(h[16],p[13])]],kU],kX=[0,kW,[1,c(l[11],0,kV),0]],kZ=a(i[1][7],kY),k0=[0,[2,[5,a(h[16],p[13])]],kZ],k2=[0,k1,[1,c(l[11],0,k0),kX]],k4=a(i[1][7],k3),k5=[0,[5,a(h[16],g[2][1])],k4],k7=[0,[0,[0,k6,[1,c(l[11],0,k5),k2]],kS],kR];L(g[10][8],af,k8,0,k7);var
b2=[0,af,aI,a2,aJ,bV,ag,bW,a3,a4,a5,b0,aj,b1];aK(349,b2,"Newring_plugin.G_newring");aK(350,[0,bg,J,b2],"Newring_plugin");return}
