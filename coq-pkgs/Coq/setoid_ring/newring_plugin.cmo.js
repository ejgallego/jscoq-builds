(function(ln){"use strict";var
bn=123,cK=",",bm="AddSetoidRing",Q='"',cJ="field_mod",u=250,cI="(",aT="vernac argument needs not wit printer.",bi="AddSetoidField",cH="constants",P="plugins/setoid_ring/g_newring.ml4",aS="Field_tac",cr="with carrier ",cF="postprocess tactic",cG="Init",aR="field",cE="InitialRing",q=246,cq="decidable",aZ=119,cB="Pphi_pow",cC='Using setoid "',cD="tactic recognizing constants",aQ="vernac argument needs not globwit printer.",I="Extension: cannot occur",cA="postprocess",bl="gen_phiZ",aY="setoid",cz="ring_mod",cp="$map",D="]",co="preprocess",bh="power_tac",cy="Print",aX="closed_term",bk="power",cx="Ring_polynom",cn=100,aW="protect_fv",ar="Ring",cm='and "',cl="$lH",cj="morphism",ck="plugins/setoid_ring/newring.ml",bj="field_lookup",ad="Coq",ch="closed",ci="F",aV="x",bg="ring_lookup",bf="ring",aU=105,cg="preprocess tactic",C="[",cv=376,cw='and morphisms "',cf="and equivalence relation ",ce=")",cb="field_mods",cc=":",cd="t",b$="abstract",ca="$f",aP="sign",b_="PEeval",aO="div",be="newring_plugin",ct="Add",cu="completeness",bd="IDphi",cs="ring_mods",b9="Pphi_dev",J=ln.jsoo_runtime,H=J.caml_check_bound,b=J.caml_new_string,t=J.caml_obj_tag,bc=J.caml_register_global,G=J.caml_wrap_exception;function
a(a,b){return a.length==1?a(b):J.caml_call_gen(a,[b])}function
c(a,b,c){return a.length==2?a(b,c):J.caml_call_gen(a,[b,c])}function
e(a,b,c,d){return a.length==3?a(b,c,d):J.caml_call_gen(a,[b,c,d])}function
O(a,b,c,d,e){return a.length==4?a(b,c,d,e):J.caml_call_gen(a,[b,c,d,e])}function
U(a,b,c,d,e,f){return a.length==5?a(b,c,d,e,f):J.caml_call_gen(a,[b,c,d,e,f])}function
lm(a,b,c,d,e,f,g,h,i){return a.length==8?a(b,c,d,e,f,g,h,i):J.caml_call_gen(a,[b,c,d,e,f,g,h,i])}var
g=J.caml_get_global_data(),bB=[0,b(be),b("get_res")],bD=[0,[0,b(ad),[0,b("Setoids"),[0,b("Setoid"),0]]],[0,[0,b(ad),[0,b("Lists"),[0,b("List"),0]]],[0,[0,b(ad),[0,b(cG),[0,b("Datatypes"),0]]],[0,[0,b(ad),[0,b(cG),[0,b("Logic"),0]]],0]]]],ax=b("setoid_ring"),E=b(be),X=g.Term,K=g.CClosure,j=g.Util,k=g.EConstr,b0=g.Tacmach,v=g.Proofview,f=g.Ltac_plugin,V=g.Coqlib,ae=g.Global,i=g.Names,o=g.Pervasives,bW=g.Lib,s=g.CamlinternalLazy,au=g.Universes,L=g.Not_found,A=g.Mod_subst,T=g.Libnames,ac=g.Retyping,bJ=g.Reductionops,d=g.Pp,z=g.CErrors,F=g.Assert_failure,as=g.Termops,aG=g.Typing,Y=g.Evarutil,bQ=g.Smartlocate,l=g.Loc,ah=g.Feedback,bM=g.Flags,ap=g.Printer,m=g.Stdarg,h=g.Genarg,av=g.Evd,bw=g.Univops,bx=g.Univ,by=g.Declare,bv=g.Constrintern,a3=g.Quote_plugin,bs=g.Tactics,bq=g.Globnames,aD=g.Summary,aE=g.Libobject,b5=g.Egramml,ak=g.Vernac_classifier,b4=g.Vernacinterp,N=g.Ppconstr,aq=g.Mltop,p=g.Pcoq,n=g.CLexer,aj=g.CList,gN=[0,0,0],gO=[0,0,0],gC=b("field kind"),gD=b(cD),gE=b(cg),gF=b(cF),gG=b(aY),gH=b(bk),gI=b(aP),gJ=b(aO),gK=b("infinite property"),gs=[0,b(aS),0],gt=[0,0,0],gu=b("field_lemmas"),gv=b("_field_lemma1"),gw=b("_field_lemma2"),gx=b("_field_lemma3"),gy=b("_field_lemma4"),gz=b("_lemma5"),gB=[22,0],gA=[22,0],gr=b("field inverse should be declared as a morphism"),gc=b("arguments of field_simplify do not have all the same type"),gd=[0,b(aR)],gb=[0,b(aS),0],ge=b(Q),gf=b(Q),gg=b("cannot find a declared field structure over"),gh=[0,b(aR)],gi=[0,b(ck),869,12],f8=[0,1],f9=[0,0],f7=b("bad field structure"),e_=[0,0,0],e$=[0,0,0],e0=b("ring kind"),e1=b(cD),e2=b(cg),e3=b(cF),e4=b(aY),e5=b(bk),e6=b(aP),e7=b(aO),eZ=b(" cannot be set twice"),eS=[0,b("Ring_base"),0],eT=b("ring_lemmas"),eU=b("_ring_lemma1"),eV=b("_ring_lemma2"),eX=[22,0],eW=[22,0],eQ=[0,1],eR=[0,0],eP=b("bad ring structure"),ey=b("ring addition should be declared as a morphism"),ez=b("ring multiplication should be declared as a morphism"),eA=b("ring opposite should be declared as a morphism"),eB=b(Q),eC=b(cm),eD=b(Q),eE=b(Q),eF=b('",'),eG=b(cw),eH=b(Q),eI=b(cC),eJ=b(Q),eK=b(cm),eL=b(Q),eM=b(cw),eN=b(Q),eO=b(cC),eu=b("cannot find setoid relation"),eg=b("arguments of ring_simplify do not have all the same type"),eh=[0,b(bf)],ei=b(Q),ej=b(Q),ek=b("cannot find a declared ring structure over"),el=[0,b(bf)],em=[0,b(ck),cv,12],dz=[0,b(cx),0],dy=b("newring"),dt=b(ar),ds=b(ar),dn=b("ring: cannot find relation (not closed)"),dm=b("ring: cannot find relation"),de=b(ar),dd=b(ar),db=b(aV),da=b(aV),c_=b(aV),c8=b("Ring.exec_tactic: anomaly"),c3=b(aV),c4=b(ci),c5=b(ci),c1=[2,1],c2=[0,1],cX=[0,b(be),b(aX)],cY=b(cd),c0=b(cd),cP=b("not found"),cQ=b("map "),cR=[0,b("lookup_map")],cN=b("dummy"),cM=b("global_head_of_constr."),df=b("Build_Setoid_Theory"),dh=b("None"),di=b("Some"),dj=b("eq"),dk=b("cons"),dl=b("nil"),dp=b(ad),dq=[0,[0,b("Ring_theory"),0],[0,[0,b(cx),0],[0,[0,b("Ring_tac"),0],[0,[0,b(cE),0],[0,[0,b(aS),0],[0,[0,b("Field_theory"),0],0]]]]]],du=[0,b(ad),0],dv=b(cE),dA=b("almost_ring_theory"),dB=b("Eqsth"),dD=b("Eq_ext"),dF=b("Eq_s_ext"),dH=b("ring_theory"),dI=b("mk_reqe"),dK=b("semi_ring_theory"),dL=b("mk_seqe"),dN=b("Abstract"),dO=b("Computational"),dQ=b("Morphism"),dS=b("inv_morph_nothing"),dT=b("mkhypo"),dV=b("hypo"),dY=b(b_),d1=b(cB),d4=b(b9),d7=b(bl),d_=b(bd),ec=b(bf),ed=b("ring-tac-carrier-table"),ee=b("ring-tac-name-table"),eo=b("tactic-new-ring-theory"),fb=[0,b(ad),0],fc=b(aS),fh=b("FEeval"),fk=b(b_),fn=b(cB),fq=b(b9),ft=b("display_pow_linear"),fw=b("display_linear"),fz=b(bl),fC=b(bd),fG=b(aR),fJ=b("PCond"),fM=b(bl),fP=b(bd),fT=b("field_cond"),fU=b(aR),fW=b("simpl_field_expr"),fY=b("almost_field_theory"),fZ=b("field_theory"),f0=b("semi_field_theory"),f1=b("AF_AR"),f3=b("F_R"),f5=b("SF_SR"),f_=b("field-tac-carrier-table"),f$=b("field-tac-name-table"),gk=b("tactic-new-field-theory"),lj=[0,b(P),1,0],lg=[0,b(P),1,0],ld=[0,b(P),1,0],lc=b("$lt"),le=[0,b(D)],lf=b(cl),lh=[0,b(C)],li=b(ca),lk=[0,b(bj)],ll=b(bj),k9=b(I),k7=b(bi),kR=b(bi),kO=b(I),kM=b(I),kK=b(bi),kH=b(I),kE=b(cf),kF=b(cr),kD=b("The following field structures have been declared:"),kC=b(I),kz=b(aQ),kx=b(aT),ki=b(aQ),kg=b(aT),j6=b(cu),j3=[0,b(P),1,0],j0=[0,b(P),1,0],jX=[0,b(P),1,0],jW=b("$lrt"),jY=[0,b(D)],jZ=b(cl),j1=[0,b(C)],j2=b(ca),j4=[0,b(bg)],j5=b(bg),jR=b(I),jP=b(bm),jz=b(bm),jw=b(I),ju=b(I),js=b(bm),jp=b(I),jm=b(cf),jn=b(cr),jl=b("The following ring structures have been declared:"),jk=b(I),jh=b(aQ),jf=b(aT),i2=b(aQ),i0=b(aT),hR=[0,0],hj=b(b$),hk=b(cq),hl=b(cj),hm=b(D),hn=b(C),ho=b(cH),hp=b(D),hq=b(C),hr=b(ch),hs=b(D),ht=b(C),hu=b(co),hv=b(D),hw=b(C),hx=b(cA),hy=b(aY),hz=b(D),hA=b(C),hB=b(bh),hC=b(D),hD=b(C),hE=b(bh),hF=b(aP),hG=b(aO),hg=[0,b(P),1,0],hd=[0,b(P),1,0],hb=[0,[0,b(D)],0],hc=b("$l"),he=[0,b(C)],hf=b("$t"),hh=[0,b(aX)],hi=b(aX),g8=b(I),g4=[0,b(P),1,0],g1=[0,b(P),1,0],gY=[0,b(P),1,0],gX=b(cp),gZ=[0,b(aW)],g0=b("$id"),g2=[0,b("in")],g3=b(cp),g5=[0,b(aW)],g6=b(aW),gS=b(I),gQ=b(I),gV=b(aW),g$=b(aX),hH=b(cz),hJ=b(cz),hO=b(cq),hS=b(b$),hW=b(cj),hZ=b(D),h2=b(C),h4=b(cH),h7=b(D),h_=b(C),ia=b(ch),id=b(D),ih=b(C),ij=b(co),im=b(D),iq=b(C),is=b(cA),ix=b(aY),iB=b(aP),iE=b(D),iH=b(C),iK=b(bk),iN=b(D),iQ=b(C),iT=b(bh),iX=b(aO),i4=b(cs),i6=b(cs),i_=b(ce),ja=b(cK),jc=b(cI),jA=[0,[0,[0,b(cy)],[0,[0,b("Rings")],0]],0],jH=[0,b(cc)],jL=[0,b(ar)],jM=[0,b(ct)],jU=b(bg),j7=b(cJ),j9=b(cJ),kd=b(cu),kk=b(cb),km=b(cb),kq=b(ce),ks=b(cK),ku=b(cI),kS=[0,[0,[0,b(cy)],[0,[0,b("Fields")],0]],0],kZ=[0,b(cc)],k3=[0,b("Field")],k4=[0,b(ct)],la=b(bj),eY=g.Option,dc=g.Refiner,c6=g.Environ,c7=g.Goal,cZ=g.CAst,cW=g.Tacticals,cS=g.Esubst,fX=g.Redexpr;function
R(b){var
c=a(d[3],b);return e(z[6],0,0,c)}function
bo(g,f,e,d,b){switch(a(f,d)){case
0:return c(K[54],e,b);case
1:return a(K[37],b);default:return-1===d?c(K[54],e,b):a(g,b)}}function
cL(b,f){var
g=c(k[81],b,f)[1];try{var
i=c(as[104],b,g)[1];return i}catch(b){b=G(b);if(b===L){var
h=a(d[3],cM);return e(z[3],0,0,h)}throw b}}function
bp(b){try{var
c=a(bq[16],b);return c}catch(b){b=G(b);if(b===L)return[0,a(i[1][6],cN)];throw b}}function
at(f,d,b){var
k=a(f,bp(b));if(k){var
p=k[1],q=-1;return bo(function(a){return at(f,d,a)},p,d,q,b)}var
h=a(X[135],b);switch(h[0]){case
6:var
r=function(a,b){return at(f,a,b)};return e(K[56],r,d,b);case
9:var
g=h[2],i=h[1],s=0;if(g.length-1<=0){var
t=a(X[aZ],[0,i,g]);return a(K[37],t)}var
l=c(j[19][51],s,g),u=l[2],m=a(X[aZ],[0,i,l[1]]),n=a(f,bp(m));if(n){var
v=n[1],o=function(b,a){return bo(function(a){return at(f,d,a)},v,d,b,a)},w=c(j[19][16],o,u),x=[6,o(-1,m),w];return a(K[38],x)}var
y=a(X[aZ],[0,i,g]);return a(K[37],y);default:return a(K[37],b)}}function
br(b,a){try{var
c=[0,e(j[17][126],bq[5],a,b)];return c}catch(a){a=G(a);if(a===L)return 0;throw a}}var
a0=[0,j[15][49][1]];function
a1(b,a){a0[1]=e(j[15][49][4],b,a,a0[1]);return 0}function
cO(f){try{var
b=c(j[15][49][22],f,a0[1]);return b}catch(b){b=G(b);if(b===L){var
g=a(d[3],cP),h=a(d[20],f),i=a(d[3],cQ),k=c(d[12],i,h),l=c(d[12],k,g);return e(z[6],0,cR,l)}throw b}}function
a2(g,f,d,b){function
h(a){return c(Y[16],d,a)}var
i=a(k[bn][1],b),j=a(cS[1],0),l=at(c(cO(g),d,b),j,i),m=e(K[42],[0,h],K[9],f),n=c(K[59],m,l);return a(k[8],n)}function
cT(a){var
b=0,c=2,d=[0,function(b,c,d){return a2(a,b,c,d)},c];return e(bs[49],0,d,b)}function
cU(b,a){var
c=[0,[0,a,0]],d=2,f=[0,function(a,c,d){return a2(b,a,c,d)},d];return e(bs[49],0,f,c)}function
cV(f,b){function
g(g){var
h=c(j[17][15],au[45],b),i=e(j[17][19],a3[1][8][4],h,a3[1][8][1]);if(e(a3[1][16],g,i,f))return a(v[13],0);var
k=a(d[7],0);return c(cW[66][4],0,k)}return c(v[68][1],v[51],g)}function
bt(b){var
d=[0,cX,0];function
e(a){return[0,c(l[10],0,a)]}var
f=c(j[17][15],e,b),g=a(h[17],m[11]),k=a(h[5],g),n=[0,[0,c(h[7],k,f)],0],o=[1,a(i[1][6],cY)],p=[0,c(cZ[1],0,o),0],q=a(h[5],m[13]),r=[0,d,[0,[0,c(h[7],q,p)],n]],s=[31,c(l[10],0,r)];return[28,[0,[0,[0,a(i[1][6],c0)],0],s]]}function
bu(d){var
b=a(ae[2],0),f=a(av[17],b),c=e(bv[13],b,f,d);return[0,c[1],c[2]]}function
S(c){var
b=a(ae[2],0),d=a(av[17],b),e=O(bv[10],b,d,0,c)[1];return a(k[8],e)}function
ab(e,d,b){var
f=a(bw[1],b),g=a(bx[38][14],d),h=c(bw[3],g,f),j=[0,a(bx[38][13],h)],k=[0,[0,lm(by[2],0,c2,0,0,0,j,0,b)],c1],l=a(i[1][6],e),m=U(by[3],0,0,l,0,k);return a(X[120],m)}function
bz(n,m){function
p(g,b){var
d=b[1],h=b[3],j=b[2],k=a(o[21],d),m=c(o[16],c3,k),f=a(i[1][6],m),n=[2,[1,c(l[10],0,f)]];return[0,d+1|0,[0,n,j],e(i[1][11][4],f,g,h)]}var
b=e(j[17][19],p,m,[0,0,0,i[1][11][1]]),q=b[3],r=b[2],s=a(i[1][6],c4),t=e(i[1][11][4],s,n,q),u=[0,t,a(f[12][32],0)[2]],d=a(i[1][6],c5),g=[0,[1,c(l[10],0,d)],r],h=[3,c(l[10],0,g)],k=[29,c(l[10],0,h)];return c(f[12][22],u,k)}var
bA=[0,[0]],c9=[0,bB,0],c$=[0,function(d,b){var
e=a(j[17][5],d),g=a(h[6],m[4]),k=c(f[12][2][7],g,e);function
l(d){var
e=b[1],f=a(o[21],d),g=c(o[16],c_,f),h=a(i[1][6],g);return c(i[1][11][22],h,e)}bA[1]=c(j[19][2],k,l);return a(v[13],0)}];e(f[6][9],0,bB,c$);function
bC(G,F,n,b,E){function
H(h,b){var
d=b[1],j=b[3],k=b[2],m=a(o[21],d),n=c(o[16],da,m),g=a(i[1][6],n),p=[2,[1,c(l[10],0,g)]],q=a(f[12][2][1],h);return[0,d+1|0,[0,p,k],e(i[1][11][4],g,q,j)]}var
p=e(j[17][19],H,E,[0,0,0,i[1][11][1]]),I=p[3],J=p[2],K=[0,I,a(f[12][32],0)[2]];function
L(b){var
d=a(o[21],b),e=c(o[16],db,d);return a(i[1][6],e)}var
M=c(j[17][54],n,L),N=a(h[5],m[4]),P=[0,c9,[0,[0,c(h[7],N,n)],0]],Q=[31,c(l[10],0,P)];function
R(a){return[0,a]}var
S=[5,[28,[0,c(j[17][15],R,M),Q]]],A=av[3][1],B=k[14],C=a(c6[10],G),g=O(c7[4][6],F,C,B,A),D=[0,g[1],g[3]],d=t(b),T=c(j[18],J,[0,S,0]),w=u===d?b[1]:q===d?a(s[2],b):b,x=[0,[0,c(l[10],0,w)],T],y=[3,c(l[10],0,x)],z=[29,c(l[10],0,y)],U=c(f[12][22],K,z),V=c(v[67][8],U,D),W=a(dc[2],V),r=a(Y[44],W),X=r[2],Z=r[1];function
_(c){var
b=a(f[12][2][2],c),d=b?a(k[bn][1],b[1]):a(o[2],c8);return a(X,d)}var
$=c(av[143],0,Z)[2];return[0,c(j[19][15],_,bA[1]),$]}function
bE(b){return[q,function(f){var
c=e(V[4],dd,bD,b),d=a(au[45],c);return a(k[8],d)}]}function
aw(a){return[q,function(b){return e(V[4],de,bD,a)}]}var
dg=bE(df),a4=aw(dh),a5=aw(di),Z=bE(dj),am=aw(dk),an=aw(dl);function
ao(b,d){var
c=t(b),e=u===c?b[1]:q===c?a(s[2],b):b;return a(k[21],[0,e,d])}function
w(f,b,e){var
d=t(b),g=u===d?b[1]:q===d?a(s[2],b):b,h=[0,c(Y[13],f,g),e];return a(k[21],h)}function
bF(f,l){var
d=c(k[3],f,l);if(9===d[0]){var
b=d[2],m=d[1];if(2<=b.length-1){var
n=[0,m,e(j[19][7],b,0,b.length-1-2|0)],g=a(k[21],n);if(c(k[aU][16],f,g)){var
h=b.length-1-1|0,i=b.length-1-2|0,o=H(b,h)[h+1];return[0,g,H(b,i)[i+1],o]}return R(dn)}}return R(dm)}var
ay=[0,dp,[0,ax,0]];function
dr(a){return c(j[18],ay,a)}var
bG=c(j[17][15],dr,dq);function
_(b){return[q,function(f){var
c=e(V[4],ds,bG,b),d=a(au[45],c);return a(k[8],d)}]}function
r(a){return[q,function(b){return e(V[4],dt,bG,a)}]}var
dw=c(j[17][15],i[1][6],[0,dv,[0,ax,du]]),dx=a(i[5][4],dw);function
bH(b){return[q,function(d){var
c=a(i[6][4],b);return e(i[13][1],[0,dx],i[5][6],c)}]}function
af(a){var
b=[0,ax,dz];return[q,function(c){return e(V[2],dy,b,a)}]}var
az=_(dA),dC=r(dB),dE=r(dD),dG=r(dF),aA=_(dH),dJ=_(dI),aB=_(dK),dM=_(dL),aC=_(dN),dP=_(dO),dR=_(dQ),$=bH(dS),dU=r(dT),y=r(dV);function
bI(g,d,f){var
b=f;for(;;){var
e=c(k[3],d,b);if(6===e[0]){var
b=e[3];continue}var
h=bF(d,b)[1],i=function(c){var
b=c[1],d=t(b),e=c[2],f=u===d?b[1]:q===d?a(s[2],b):b;return[0,f,e]},l=c(j[17][15],i,g),m=function(a){return-1===a?1:2},n=[0,[0,cL(d,h),m],l];return function(a){return br(n,a)}}}var
dW=0;function
dX(b){var
a=b+1|0;if(!(14<a>>>0))switch(a){case
9:case
13:return 2;case
0:case
11:case
14:return 0}return 1}var
dZ=[0,[0,af(dY),dX],dW];function
d0(b){var
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
d2=[0,[0,af(d1),d0],dZ];function
d3(a){if(11<=a)if(14<=a)var
b=15<=a?0:1;else{if(12!==a)return 2;var
b=1}else
var
b=-1===a?1:8<=a?1:0;return b?0:1}var
d5=[0,[0,af(d4),d3],d2];function
d6(a){return 0}var
d8=[0,[0,r(d7),d6],d5];function
d9(a){return 0}var
d$=[0,[0,r(d_),d9],d8],ea=[0,[0,an,function(a){return-1===a?0:1}],d$],eb=[0,[0,am,function(a){return-1===a?0:2===a?2:1}],ea];a1(ec,function(a,b){return bI(eb,a,b)});function
W(b){return a(ap[15],b)}var
ag=a(j[21][1],[0,X[136]]),a6=e(aD[2],0,ed,ag[1]),a7=e(aD[2],0,ee,T[24][1]);function
ef(a){return c(ag[22],a,a6[1])}function
en(d){var
a=d[2],b=d[1],e=c(A[45],b,a[1]),g=c(A[45],b,a[2]),h=c(A[45],b,a[3]),i=c(A[45],b,a[4]),j=c(A[45],b,a[5]),k=c(A[45],b,a[6]),l=c(A[45],b,a[9]),m=c(A[45],b,a[10]),n=c(f[5][1],b,a[7]),o=c(f[5][1],b,a[8]),p=c(f[5][1],b,a[11]),q=c(f[5][1],b,a[12]);if(e===a[1])if(g===a[2])if(c(X[131],h,a[3]))if(i===a[4])if(j===a[5])if(k===a[6])if(l===a[9])if(m===a[10])if(n===a[7])if(o===a[8])if(p===a[11])if(q===a[12])return a;return[0,e,g,h,i,j,k,n,o,l,m,p,q]}function
bK(b){var
a=b[2],c=b[1][1];a6[1]=e(ag[4],a[1],a,a6[1]);a7[1]=e(T[24][4],c,a,a7[1]);return 0}var
aF=a(aE[1],eo),ep=aF[8],eq=aF[7];function
er(a){return[0,a]}function
es(c,b){var
a=1===c?1:0;return a?bK(b):a}var
et=a(aE[4],[0,aF[1],bK,aF[3],es,er,en,eq,ep]);function
bL(c,d,b,a){try{var
e=O(f[22][11],c,d[1],b,a),i=e[2],g=O(f[22][12],c,e[1],b,a),j=g[2],h=O(f[22][13],c,g[1],b,a),k=h[2];d[1]=h[1];var
l=ao(dg,[0,b,a,i,j,k]);return l}catch(a){a=G(a);if(a===L)return R(eu);throw a}}function
ev(h,g,f,e,d,c,b,a){return ao(dJ,[0,h,g,f,e,d,c,b,a])}function
ew(f,e,d,c,b,a){return ao(dM,[0,f,e,d,c,b,a])}function
ex(r,h,i){var
b=i[5],m=i[4],j=i[3],l=i[2],g=i[1],p=c(k[3],h[1],b);if(9===p[0])if(1===p[2].length-1){var
D=t(Z),aR=p[1],aS=u===D?Z[1]:q===D?a(s[2],Z):Z;if(e(k[94],h[1],aR,aS)){var
aT=w(h,dC,[0,g]),aU=m?w(h,dE,[0,g,l,j,m[1]]):w(h,dG,[0,g,l,j]),aV=e(aG[7],r,h,aT);return[0,aV,e(aG[7],r,h,aU)]}}var
v=[0,[0,[0,[0,g,[0,b]]],[0,[0,[0,g,[0,b]]],0]],[0,[0,g,[0,b]]]],E=bL(a(ae[2],0),h,g,b);try{var
aQ=c(f[22][14],v,l),x=aQ}catch(a){a=G(a);if(a!==L)throw a;var
x=R(ey)}var
n=x[2];try{var
aP=c(f[22][14],v,j),y=aP}catch(a){a=G(a);if(a!==L)throw a;var
y=R(ez)}var
o=y[2];if(m){var
z=m[1];try{var
aq=c(f[22][14],[0,[0,[0,[0,g,[0,b]]],0],[0,[0,g,[0,b]]]],z),A=aq}catch(a){a=G(a);if(a!==L)throw a;var
A=R(eA)}var
B=A[2],F=ev(g,l,j,z,b,n,o,B),H=a(d[3],eB),I=W(B),J=a(d[3],eC),K=a(d[13],0),M=a(d[3],eD),N=W(o),O=a(d[3],eE),P=a(d[13],0),Q=a(d[3],eF),S=W(n),T=a(d[3],eG),U=a(d[13],0),V=a(d[3],eH),X=W(b),Y=a(d[3],eI),_=c(d[12],Y,X),$=c(d[12],_,V),aa=c(d[12],$,U),ab=c(d[12],aa,T),ac=c(d[12],ab,S),ad=c(d[12],ac,Q),af=c(d[12],ad,P),ag=c(d[12],af,O),ai=c(d[12],ag,N),aj=c(d[12],ai,M),ak=c(d[12],aj,K),al=c(d[12],ak,J),am=c(d[12],al,I),an=c(d[12],am,H),ao=ah[6],ap=function(a){return c(ao,0,a)};c(bM[47],ap,an);var
C=F}else{var
ar=a(d[3],eJ),as=W(o),at=a(d[3],eK),au=a(d[13],0),av=a(d[3],eL),aw=W(n),ax=a(d[3],eM),ay=a(d[13],0),az=a(d[3],eN),aA=W(b),aB=a(d[3],eO),aC=c(d[12],aB,aA),aD=c(d[12],aC,az),aE=c(d[12],aD,ay),aF=c(d[12],aE,ax),aH=c(d[12],aF,aw),aI=c(d[12],aH,av),aJ=c(d[12],aI,au),aK=c(d[12],aJ,at),aL=c(d[12],aK,as),aM=c(d[12],aL,ar),aN=ah[6],aO=function(a){return c(aN,0,a)};c(bM[47],aO,aM);var
C=ew(g,l,j,b,n,o)}return[0,E,C]}function
bN(h,g,f,e,d,c,b,a){return a?a[1]:ex(h,g,[0,f,e,d,c,b])}function
bO(b){if(typeof
b==="number"){var
c=t(aC);return u===c?aC[1]:q===c?a(s[2],aC):aC}else
return 0===b[0]?ao(dP,[0,b[1]]):ao(dR,[0,b[1]])}function
bP(w,v,r,p,o,d){if(d){var
b=d[1];if(0===b[0])return a(f[8][3],b[1]);var
g=b[1],h=bQ[3],i=function(a){return c(h,0,a)};return bt(c(j[17][15],i,g))}var
e=t($),k=u===e?$[1]:q===e?a(s[2],$):$,m=[0,[0,c(l[10],0,k)],0],n=[3,c(l[10],0,m)];return[29,c(l[10],0,n)]}function
aH(c,b,a){return w(b,dU,[0,U(ac[2],0,0,c,b[1],a),a])}function
bR(d,b,h){var
f=t(y),i=u===f?y[1]:q===f?a(s[2],y):y,g=c(Y[13],b,i),l=w(b,an,[0,g]);function
m(c,a){return w(b,am,[0,g,aH(d,b,c),a])}var
n=e(j[17][19],m,h,l),o=e(aG[7],d,b,n),p=a(k[bn][1],o);return c(Y[43],b[1],p)}function
bS(n,b,e){var
g=t(y),o=u===g?y[1]:q===g?a(s[2],y):y,h=c(Y[13],b,o);if(e){var
i=e[1],d=i[1],p=i[2];if(0===d[0])var
k=a(f[8][3],d[1]);else
var
r=d[1],v=bQ[3],x=function(a){return c(v,0,a)},k=bt(c(j[17][15],x,r));return[0,k,w(b,a5,[0,h,aH(n,b,S(p))])]}var
m=t($),z=u===m?$[1]:q===m?a(s[2],$):$,A=[0,c(l[10],0,z)],B=w(b,a4,[0,h]),C=[3,c(l[10],0,[0,A,0])];return[0,[29,c(l[10],0,C)],B]}function
bT(g,b,d){var
e=t(y),h=u===e?y[1]:q===e?a(s[2],y):y,f=c(Y[13],b,h);return d?w(b,a5,[0,f,aH(g,b,S(d[1]))]):w(b,a4,[0,f])}function
bU(g,b,d){var
e=t(y),h=u===e?y[1]:q===e?a(s[2],y):y,f=c(Y[13],b,h);return d?w(b,a5,[0,f,aH(g,b,S(d[1]))]):w(b,a4,[0,f])}function
bV(v,L,ar,K,aq,J,ap,ao,an){var
M=J[2],N=J[1],O=L[2],g=L[1],as=c(j[18],ay,eS);a(V[3],as);var
l=a(ae[2],0),aa=U(ac[2],0,0,l,g,O),p=c(k[3],g,aa);if(9===p[0]){var
b=p[2],w=b.length-1-6|0;if(2<w>>>0)var
h=0;else{var
r=p[1];switch(w){case
0:var
x=t(aB),ad=b[1],af=b[2],ag=b[3],ah=b[4],ai=b[5],aj=b[6],ak=u===x?aB[1]:q===x?a(s[2],aB):aB;if(e(k[94],g,r,ak))var
d=[0,eQ,ad,af,ag,ah,ai,0,0,aj],h=1;else
var
h=0;break;case
1:var
h=0;break;default:var
y=b[1],z=b[2],A=b[3],B=b[4],C=b[5],D=b[6],E=b[7],F=b[8],G=t(az),al=u===G?az[1]:q===G?a(s[2],az):az;if(e(k[94],g,r,al))var
d=[0,0,y,z,A,B,C,[0,D],[0,E],F],h=1;else{var
I=t(aA),am=u===I?aA[1]:q===I?a(s[2],aA):aA;if(e(k[94],g,r,am))var
d=[0,eR,y,z,A,B,C,[0,D],[0,E],F],h=1;else
var
h=0}}}}else
var
h=0;if(!h)var
d=R(eP);var
P=d[9],Q=d[8],S=d[6],T=d[5],W=d[2],m=[0,g],at=d[4],au=d[3],av=d[1],X=bN(l,m,W,T,S,Q,P,ar),Y=X[1],aw=X[2],Z=bS(l,m,ap),ax=Z[2],aC=Z[1],aD=bT(l,m,ao),aE=bU(l,m,an),aF=[0,Y,[0,aw,[0,O,[0,ax,[0,aD,[0,aE,[0,bO(K),0]]]]]]],aG=bH(eT),_=bC(l,m[1],5,aG,aF),$=_[2],n=_[1],aH=H(n,3)[4],aI=H(n,4)[5],aJ=a(i[1][8],v),aK=ab(c(o[16],aJ,eU),$,aH),aL=a(i[1][8],v),aM=ab(c(o[16],aL,eV),$,aI),aN=bP(l,g,K,av,[0,au,at,T,S,Q],aq),aO=N?a(f[8][3],N[1]):eX,aP=M?a(f[8][3],M[1]):eW,aQ=c(k[5],g,W),aR=c(k[5],g,P),aS=c(k[5],g,Y),aT=H(n,0)[1],aU=H(n,2)[3],aV=a(et,[0,aQ,aR,aS,H(n,1)[2],aU,aT,aN,aC,aK,aM,aO,aP]);c(bW[6],v,aV);return 0}function
bX(a){return typeof
a==="number"?0:0===a[0]?[0,S(a[1])]:[1,S(a[1])]}function
x(e,b,d){return a(eY[3],b[1])?(b[1]=[0,d],0):R(c(o[16],e,eZ))}function
e8(q,p,o){var
l=bu(p),b=[0,0],d=[0,0],e=[0,0],f=[0,0],g=[0,0],h=[0,0],i=[0,0],k=[0,0],r=l[2],s=l[1];function
m(a){switch(a[0]){case
0:return x(e0,b,bX(a[1]));case
1:return x(e1,e,a[1]);case
2:return x(e2,f,a[1]);case
3:return x(e3,g,a[1]);case
4:var
c=a[1],j=S(a[2]);return x(e4,d,[0,S(c),j]);case
5:return x(e5,i,[0,a[1],a[2]]);case
6:return x(e6,h,a[1]);default:return x(e7,k,a[1])}}c(j[17][14],m,o);var
a=b[1],n=a?a[1]:0;return bV(q,[0,s,r],d[1],n,e[1],[0,f[1],g[1]],i[1],h[1],k[1])}function
bY(d,a,c){if(a)return a;var
b=bF(d,c);return[0,b[2],[0,b[3],0]]}function
bZ(d,a,b,c){var
f=w(a,an,[0,b]);function
g(d,c){return w(a,am,[0,b,d,c])}var
h=e(j[17][19],g,c,f);return e(aG[7],d,a,h)}function
B(b){var
c=a(k[8],b);return a(f[12][2][1],c)}function
aa(b){var
d=a(f[12][32],0);return c(f[12][2][6],d,b)}function
e9(a){var
b=B(a[2]),c=B(a[3]),d=B(a[4]),e=B(a[5]),f=B(a[6]),g=aa(a[7]),h=aa(a[8]),i=B(a[9]),j=B(a[10]),k=aa([28,[0,e_,a[11]]]);return[0,b,[0,c,[0,d,[0,e,[0,f,[0,g,[0,h,[0,i,[0,j,[0,k,[0,aa([28,[0,e$,a[12]]]),0]]]]]]]]]]]}function
fa(I,H,E,D){function
b(n){var
b=a(b0[48][4],n),g=a(v[63][5],n);try{var
h=bY(b,E,D),l=[0,b];if(h){var
o=h[2],i=U(ac[2],0,0,g,b,h[1]),p=function(f){var
h=U(ac[2],0,0,g,b,f),c=1-U(bJ[81],0,g,b,i,h);if(c){var
j=a(d[3],eg);return e(z[6],0,eh,j)}return c};c(j[17][14],p,o);try{var
C=ef(c(k[5],b,i)),m=C}catch(b){b=G(b);if(b!==L)throw b;var
q=a(d[3],ei),r=W(i),s=a(d[3],ej),t=a(d[13],0),u=a(d[3],ek),w=c(d[12],u,t),x=c(d[12],w,s),y=c(d[12],x,r),A=c(d[12],y,q),m=e(z[6],0,el,A)}var
J=bZ(g,l,a(k[8],m[1]),h),K=a(f[12][2][1],J),M=B(bR(g,l,H)),N=e9(m),O=bz(I,c(j[18],N,[0,M,[0,K,0]])),P=a(v[61][1],l[1]),Q=c(v[15],P,O);return Q}throw[0,F,em]}catch(b){b=G(b);if(a(v[67][10],b))return c(v[18],0,b);throw b}}return a(v[63][9],b)}var
fd=c(j[17][15],i[1][6],[0,fc,[0,ax,fb]]),fe=a(i[5][4],fd),ff=0;function
fg(b){var
a=b+1|0;if(!(16<a>>>0))switch(a){case
11:case
15:return 2;case
0:case
13:case
16:return 0}return 1}var
fi=[0,[0,r(fh),fg],ff];function
fj(b){var
a=b+1|0;if(!(14<a>>>0))switch(a){case
9:case
13:return 2;case
0:case
11:case
14:return 0}return 1}var
fl=[0,[0,af(fk),fj],fi];function
fm(b){var
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
fo=[0,[0,af(fn),fm],fl];function
fp(a){if(11<=a)if(14<=a)var
b=15<=a?0:1;else{if(12!==a)return 2;var
b=1}else
var
b=-1===a?1:8<=a?1:0;return b?0:1}var
fr=[0,[0,af(fq),fp],fo];function
fs(b){var
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
fu=[0,[0,r(ft),fs],fr];function
fv(a){if(12<=a)if(15<=a)var
b=17<=a?0:1;else{if(13!==a)return 2;var
b=1}else
var
b=-1===a?1:9<=a?1:0;return b?0:1}var
fx=[0,[0,r(fw),fv],fu];function
fy(a){return 0}var
fA=[0,[0,r(fz),fy],fx];function
fB(a){return 0}var
fD=[0,[0,r(fC),fB],fA],fE=[0,[0,an,function(a){return-1===a?0:1}],fD],fF=[0,[0,am,function(a){return-1===a?0:2===a?2:1}],fE];a1(fG,function(a,b){return bI(fF,a,b)});var
fH=0;function
fI(b){var
a=b+1|0;if(!(15<a>>>0))switch(a){case
10:case
14:return 2;case
0:case
12:case
15:return 0}return 1}var
fK=[0,[0,r(fJ),fI],fH];function
fL(a){return 0}var
fN=[0,[0,r(fM),fL],fK];function
fO(a){return 0}var
fQ=[0,[0,r(fP),fO],fN],fR=[0,[0,an,function(a){return-1===a?0:1}],fQ],fS=[0,[0,am,function(a){return-1===a?0:2===a?2:1}],fR];a1(fT,function(e,f){function
b(c){var
b=c[1],d=t(b),e=c[2],f=u===d?b[1]:q===d?a(s[2],b):b;return[0,f,e]}var
d=c(j[17][15],b,fS);return function(a){return br(d,a)}});function
fV(a,b,c){return a2(fU,a,b,c)}c(fX[3],fW,fV);var
aI=r(fY),aJ=r(fZ),aK=r(f0),f2=r(f1),f4=r(f3),f6=r(f5),a8=e(aD[2],0,f_,ag[1]),a9=e(aD[2],0,f$,T[24][1]);function
ga(a){return c(ag[22],a,a8[1])}function
gj(d){var
a=d[2],b=d[1],e=c(A[45],b,a[1]),g=c(A[45],b,a[2]),h=c(A[45],b,a[5]),i=c(A[45],b,a[6]),j=c(A[45],b,a[7]),k=c(A[45],b,a[8]),l=c(A[45],b,a[9]),m=c(f[5][1],b,a[3]),n=c(f[5][1],b,a[4]),o=c(f[5][1],b,a[10]),p=c(f[5][1],b,a[11]);if(e===a[1])if(g===a[2])if(h===a[5])if(i===a[6])if(j===a[7])if(k===a[8])if(l===a[9])if(m===a[3])if(n===a[4])if(o===a[10])if(p===a[11])return a;return[0,e,g,m,n,h,i,j,k,l,o,p]}function
b1(b){var
a=b[2],c=b[1][1];a8[1]=e(ag[4],a[1],a,a8[1]);a9[1]=e(T[24][4],c,a,a9[1]);return 0}var
aL=a(aE[1],gk),gl=aL[8],gm=aL[7];function
gn(a){return[0,a]}function
go(c,b){var
a=1===c?1:0;return a?b1(b):a}var
gp=a(aE[4],[0,aL[1],b1,aL[3],go,gn,gj,gm,gl]);function
gq(g,b,i,d){var
h=c(k[3],g[1],d);if(9===h[0])if(1===h[2].length-1){var
l=t(Z),o=h[1],p=u===l?Z[1]:q===l?a(s[2],Z):Z;if(e(k[94],g[1],o,p)){var
r=a(V[36],0)[6],v=a(au[45],r),w=[0,a(k[8],v),[0,b,b,i]];return a(k[21],w)}}bL(a(ae[2],0),g,b,d);var
m=[0,[0,[0,[0,b,[0,d]]],0],[0,[0,b,[0,d]]]];try{var
n=c(f[22][14],m,i),j=n}catch(a){a=G(a);if(a!==L)throw a;var
j=R(gr)}return j[2]}function
gL(m,br,bq){var
aq=[0,0],ar=[0,0],at=[0,0],au=[0,0],av=[0,0],aw=[0,0],ax=[0,0],az=[0,0],aA=[0,0];function
bo(b){if(0===b[0]){var
a=b[1];switch(a[0]){case
0:return x(gC,aq,bX(a[1]));case
1:return x(gD,at,a[1]);case
2:return x(gE,au,a[1]);case
3:return x(gF,av,a[1]);case
4:var
c=a[1],d=S(a[2]);return x(gG,ar,[0,S(c),d]);case
5:return x(gH,az,[0,a[1],a[2]]);case
6:return x(gI,ax,a[1]);default:return x(gJ,aA,a[1])}}return x(gK,aw,S(b[1]))}c(j[17][14],bo,bq);var
aB=aq[1],N=aB?aB[1]:0,aC=aA[1],aD=ax[1],aE=az[1],aF=av[1],aG=au[1],aH=at[1],aL=aw[1],bp=ar[1],aR=c(j[18],ay,gs);a(V[3],aR);var
ag=bu(br),p=ag[2],r=ag[1],l=a(ae[2],0),d=[0,r],aN=U(ac[2],0,0,l,d[1],p),J=c(k[3],d[1],aN);if(9===J[0]){var
b=J[2],O=b.length-1-8|0;if(2<O>>>0)var
h=0;else{var
K=J[1];switch(O){case
0:var
P=b[1],Q=b[2],T=b[3],W=b[4],Y=b[5],Z=b[6],_=b[7],$=b[8],aa=t(aK),aO=u===aa?aK[1]:q===aa?a(s[2],aK):aK;if(e(as[aU],d[1],aO,K))var
g=[0,f8,P,Q,T,W,Y,0,0,Z,_,$,w(d,f6,[0,P,Q,T,W,Y,Z,_,$,p])],h=1;else
var
h=0;break;case
1:var
h=0;break;default:var
y=b[1],z=b[2],A=b[3],B=b[4],C=b[5],D=b[6],E=b[7],F=b[8],G=b[9],I=b[10],ad=t(aI),aP=u===ad?aI[1]:q===ad?a(s[2],aI):aI;if(e(as[aU],d[1],aP,K))var
g=[0,0,y,z,A,B,C,[0,D],[0,E],F,G,I,w(d,f2,[0,y,z,A,B,C,D,E,F,G,I,p])],h=1;else{var
af=t(aJ),aQ=u===af?aJ[1]:q===af?a(s[2],aJ):aJ;if(e(as[aU],d[1],aQ,K))var
g=[0,f9,y,z,A,B,C,[0,D],[0,E],F,G,I,w(d,f4,[0,y,z,A,B,C,D,E,F,G,I,p])],h=1;else
var
h=0}}}}else
var
h=0;if(!h)var
g=R(f7);var
L=g[11],ah=g[8],ai=g[6],aj=g[5],M=g[2],aS=g[12],aT=g[10],aV=g[4],aW=g[3],aX=g[1],ak=bN(l,d,M,aj,ai,ah,L,bp),al=ak[2],am=ak[1];bV(m,[0,d[1],aS],[0,[0,am,al]],N,aH,gt,aE,aD,aC);var
an=bS(l,d,aE),aY=an[2],a0=an[1],a1=bT(l,d,aD),a2=bU(l,d,aC),a3=gq(d,M,aT,L),a4=[0,am,[0,al,[0,a3,[0,p,[0,aY,[0,a1,[0,a2,[0,bO(N),0]]]]]]]],aM=[q,function(c){var
b=a(i[6][4],gu);return e(i[13][1],[0,fe],i[5][6],b)}],ao=bC(l,d[1],9,aM,a4),v=ao[2],n=ao[1],a5=H(n,3)[4],a6=H(n,4)[5],a7=H(n,5)[6],a8=H(n,6)[7];if(aL)var
a9=[0,c(k[5],r,aL[1])],a_=[0,H(n,8)[9],a9],ap=a(X[aZ],a_);else
var
ap=H(n,7)[8];var
a$=a(i[1][8],m),ba=ab(c(o[16],a$,gv),v,a5),bb=a(i[1][8],m),bc=ab(c(o[16],bb,gw),v,a6),bd=a(i[1][8],m),be=ab(c(o[16],bd,gx),v,a7),bf=a(i[1][8],m),bg=ab(c(o[16],bf,gy),v,a8),bh=a(i[1][8],m),bi=ab(c(o[16],bh,gz),v,ap),bj=bP(l,r,N,aX,[0,aW,aV,aj,ai,ah],aH),bk=aG?a(f[8][3],aG[1]):gB,bl=aF?a(f[8][3],aF[1]):gA,bm=c(k[5],r,M),bn=a(gp,[0,bm,c(k[5],r,L),bj,a0,ba,bc,be,bg,bi,bk,bl]);c(bW[6],m,bn);return 0}function
gM(a){var
b=B(a[2]),c=aa(a[3]),d=aa(a[4]),e=B(a[5]),f=B(a[7]),g=B(a[6]),h=B(a[8]),i=B(a[9]),j=aa([28,[0,gN,a[10]]]);return[0,b,[0,c,[0,d,[0,e,[0,f,[0,g,[0,h,[0,i,[0,j,[0,aa([28,[0,gO,a[11]]]),0]]]]]]]]]]}var
M=[0,cU,cT,cV,e8,a7,fa,gL,a9,function(J,I,H,E){function
b(n){var
b=a(b0[48][4],n),g=a(v[63][5],n);try{var
h=bY(b,H,E),l=[0,b],o=c(j[18],ay,gb);a(V[3],o);if(h){var
p=h[2],i=U(ac[2],0,0,g,b,h[1]),q=function(f){var
h=U(ac[2],0,0,g,b,f),c=1-U(bJ[81],0,g,b,i,h);if(c){var
j=a(d[3],gc);return e(z[6],0,gd,j)}return c};c(j[17][14],q,p);try{var
D=ga(c(k[5],b,i)),m=D}catch(b){b=G(b);if(b!==L)throw b;var
r=a(d[3],ge),s=W(i),t=a(d[3],gf),u=a(d[13],0),w=a(d[3],gg),x=c(d[12],w,u),y=c(d[12],x,t),A=c(d[12],y,s),C=c(d[12],A,r),m=e(z[6],0,gh,C)}var
K=bZ(g,l,a(k[8],m[1]),h),M=a(f[12][2][1],K),N=B(bR(g,l,I)),O=gM(m),P=bz(J,c(j[18],O,[0,N,[0,M,0]])),Q=a(v[61][1],l[1]),R=c(v[15],Q,P);return R}throw[0,F,gi]}catch(b){b=G(b);if(a(v[67][10],b))return c(v[18],0,b);throw b}}return a(v[63][9],b)}];bc(366,M,"Newring_plugin.Newring");a(aq[12],E);var
gP=0,gR=[0,function(b){if(b)if(!b[2]){var
d=b[1],e=a(h[6],m[5]),g=c(f[12][2][7],e,d);return function(b){return a(M[2],g)}}return a(o[2],gQ)},gP],gT=[0,function(b){if(b){var
d=b[2];if(d)if(!d[2]){var
e=d[1],g=b[1],i=a(h[6],m[5]),j=c(f[12][2][7],i,g),k=a(h[6],m[9]),l=c(f[12][2][7],k,e);return function(a){return c(M[1],j,l)}}}return a(o[2],gS)},gR],gU=a(j[19][12],gT);e(f[6][9],0,[0,E,gV],gU);function
gW(t){var
k=[0,a(i[1][7],gX)],b=m[5],h=0,j=0;if(0===b[0]){var
n=[0,[0,gZ,[0,[1,c(l[10],0,[0,[5,[0,b[1]]],k])],j]],h],p=[0,a(i[1][7],g0)],d=m[9],o=0;if(0===d[0]){var
q=[0,g2,[0,[1,c(l[10],0,[0,[5,[0,d[1]]],p])],o]],r=[0,a(i[1][7],g3)],g=m[5];if(0===g[0]){var
s=[0,[0,g5,[0,[1,c(l[10],0,[0,[5,[0,g[1]]],r])],q]],n];return e(f[9][4],[0,E,g6],0,s)}throw[0,F,g4]}throw[0,F,g1]}throw[0,F,gY]}c(aq[19],gW,E);var
g7=0,g9=[0,function(b){if(b){var
d=b[2];if(d)if(!d[2]){var
e=d[1],g=b[1],i=a(h[6],m[13]),j=c(f[12][2][7],i,g),k=a(h[17],m[23]),l=a(h[6],k),n=c(f[12][2][7],l,e);return function(a){return c(M[3],j,n)}}}return a(o[2],g8)},g7],g_=a(j[19][12],g9);e(f[6][9],0,[0,E,g$],g_);function
ha(o){var
h=[0,a(i[1][7],hc)],b=m[23],g=0;if(0===b[0]){var
j=[0,he,[0,[1,c(l[10],0,[0,[0,[5,[0,b[1]]]],h])],hb]],k=[0,a(i[1][7],hf)],d=m[13];if(0===d[0]){var
n=[0,[0,hh,[0,[1,c(l[10],0,[0,[5,[0,d[1]]],k])],j]],g];return e(f[9][4],[0,E,hi],0,n)}throw[0,F,hg]}throw[0,F,hd]}c(aq[19],ha,E);function
aM(b){switch(b[0]){case
0:var
g=b[1];if(typeof
g==="number")return a(d[3],hj);else{if(0===g[0]){var
j=c(d[31],N[23],g[1]),k=a(d[3],hk);return c(d[12],k,j)}var
l=c(d[31],N[23],g[1]),m=a(d[3],hl);return c(d[12],m,l)}case
1:var
h=b[1];if(0===h[0]){var
n=h[1],o=a(d[3],hm),p=a(f[2][19],n),q=a(d[3],hn),r=a(d[13],0),s=a(d[3],ho),t=c(d[12],s,r),u=c(d[12],t,q),v=c(d[12],u,p);return c(d[12],v,o)}var
w=h[1],x=a(d[3],hp),y=e(d[38],d[13],T[41],w),z=a(d[3],hq),A=a(d[13],0),B=a(d[3],hr),C=c(d[12],B,A),D=c(d[12],C,z),E=c(d[12],D,y);return c(d[12],E,x);case
2:var
F=b[1],G=a(d[3],hs),H=a(f[2][19],F),I=a(d[3],ht),J=a(d[13],0),K=a(d[3],hu),L=c(d[12],K,J),M=c(d[12],L,I),O=c(d[12],M,H);return c(d[12],O,G);case
3:var
P=b[1],Q=a(d[3],hv),R=a(f[2][19],P),S=a(d[3],hw),U=a(d[13],0),V=a(d[3],hx),W=c(d[12],V,U),X=c(d[12],W,S),Y=c(d[12],X,R);return c(d[12],Y,Q);case
4:var
Z=b[1],_=c(d[31],N[23],b[2]),$=c(d[31],N[23],Z),aa=a(d[3],hy),ab=c(d[12],aa,$);return c(d[12],ab,_);case
5:var
i=b[1];if(0===i[0]){var
ac=b[2],ad=i[1],ae=a(d[3],hz),af=a(f[2][19],ad),ag=a(d[3],hA),ah=a(d[13],0),ai=c(d[31],N[23],ac),aj=a(d[3],hB),ak=c(d[12],aj,ai),al=c(d[12],ak,ah),am=c(d[12],al,ag),an=c(d[12],am,af);return c(d[12],an,ae)}var
ao=b[2],ap=i[1],aq=a(d[3],hC),ar=e(d[38],d[13],T[41],ap),as=a(d[3],hD),at=a(d[13],0),au=c(d[31],N[23],ao),av=a(d[3],hE),aw=c(d[12],av,au),ax=c(d[12],aw,at),ay=c(d[12],ax,as),az=c(d[12],ay,ar);return c(d[12],az,aq);case
6:var
aA=c(d[31],N[23],b[1]),aB=a(d[3],hF);return c(d[12],aB,aA);default:var
aC=c(d[31],N[23],b[1]),aD=a(d[3],hG);return c(d[12],aD,aC)}}var
a_=a(h[3],hH),hI=a(h[4],a_),aN=e(p[13],p[9],hJ,hI),hK=0,hL=0;function
hM(a,c,b){return[0,[0,a]]}var
hN=[6,p[15][1]],hP=[0,[0,[0,[0,0,[0,a(n[11],hO)]],hN],hM],hL];function
hQ(b,a){return hR}var
hT=[0,[0,[0,0,[0,a(n[11],hS)]],hQ],hP];function
hU(a,c,b){return[0,[1,a]]}var
hV=[6,p[15][1]],hX=[0,[0,[0,[0,0,[0,a(n[11],hW)]],hV],hU],hT];function
hY(e,a,d,c,b){return[1,[0,a]]}var
h0=[0,a(n[11],hZ)],h1=[6,f[3][18]],h3=[0,a(n[11],h2)],h5=[0,[0,[0,[0,[0,[0,0,[0,a(n[11],h4)]],h3],h1],h0],hY],hX];function
h6(e,a,d,c,b){return[1,[1,a]]}var
h8=[0,a(n[11],h7)],h9=[1,[6,p[15][7]]],h$=[0,a(n[11],h_)],ib=[0,[0,[0,[0,[0,[0,0,[0,a(n[11],ia)]],h$],h9],h8],h6],h5];function
ic(e,a,d,c,b){return[2,a]}var
ie=[0,a(n[11],id)],ig=[6,f[3][18]],ii=[0,a(n[11],ih)],ik=[0,[0,[0,[0,[0,[0,0,[0,a(n[11],ij)]],ii],ig],ie],ic],ib];function
il(e,a,d,c,b){return[3,a]}var
io=[0,a(n[11],im)],ip=[6,f[3][18]],ir=[0,a(n[11],iq)],it=[0,[0,[0,[0,[0,[0,0,[0,a(n[11],is)]],ir],ip],io],il],ik];function
iu(b,a,d,c){return[4,a,b]}var
iv=[6,p[15][1]],iw=[6,p[15][1]],iy=[0,[0,[0,[0,[0,0,[0,a(n[11],ix)]],iw],iv],iu],it];function
iz(a,c,b){return[6,a]}var
iA=[6,p[15][1]],iC=[0,[0,[0,[0,0,[0,a(n[11],iB)]],iA],iz],iy];function
iD(f,b,e,a,d,c){return[5,[1,b],a]}var
iF=[0,a(n[11],iE)],iG=[1,[6,p[15][7]]],iI=[0,a(n[11],iH)],iJ=[6,p[15][1]],iL=[0,[0,[0,[0,[0,[0,[0,0,[0,a(n[11],iK)]],iJ],iI],iG],iF],iD],iC];function
iM(f,b,e,a,d,c){return[5,[0,b],a]}var
iO=[0,a(n[11],iN)],iP=[6,f[3][18]],iR=[0,a(n[11],iQ)],iS=[6,p[15][1]],iU=[0,[0,[0,[0,[0,[0,[0,0,[0,a(n[11],iT)]],iS],iR],iP],iO],iM],iL];function
iV(a,c,b){return[7,a]}var
iW=[6,p[15][1]],iY=[0,0,[0,[0,0,0,[0,[0,[0,[0,0,[0,a(n[11],iX)]],iW],iV],iU]],hK]];e(p[22],aN,0,iY);function
iZ(h,g,f,c){var
b=a(d[3],i0);return e(z[3],0,0,b)}function
i1(h,g,f,c){var
b=a(d[3],i2);return e(z[3],0,0,b)}function
i3(c,b,a){return aM}O(f[2][1],a_,i3,i1,iZ);function
b2(b){var
c=e(d[38],d[28],aM,b);return a(d[45],c)}var
ai=a(h[3],i4),i5=a(h[4],ai),b3=e(p[13],p[9],i6,i5),i7=0,i8=0;function
i9(d,a,c,b){return a}var
i$=[0,a(n[11],i_)],jb=[2,[6,aN],[0,a(n[11],ja)]],jd=[0,0,[0,[0,0,0,[0,[0,[0,[0,[0,0,[0,a(n[11],jc)]],jb],i$],i9],i8]],i7]];e(p[22],b3,0,jd);function
je(h,g,f,c){var
b=a(d[3],jf);return e(z[3],0,0,b)}function
jg(h,g,f,c){var
b=a(d[3],jh);return e(z[3],0,0,b)}function
ji(c,b,a){return b2}O(f[2][1],ai,ji,jg,je);var
jj=0,jo=[0,[0,0,function(b){return b?a(o[2],jk):function(g){var
b=a(d[22],jl);c(ah[7],0,b);var
e=M[5][1];function
f(e,b){var
f=a(ap[8],b[2]),g=a(d[3],jm),h=a(d[13],0),i=a(ap[8],b[1]),j=a(d[3],jn),k=a(d[13],0),l=a(T[20],e),m=a(N[12],l),n=c(d[12],m,k),o=c(d[12],n,j),p=c(d[12],o,i),q=c(d[12],p,h),r=c(d[12],q,g),s=c(d[12],r,f),t=c(d[26],2,s);return c(ah[7],0,t)}return c(T[24][10],f,e)}}],jj],jq=[0,[0,0,function(b){if(b){var
d=b[2];if(d){var
f=d[2];if(f)if(!f[2]){var
i=f[1],j=d[1],k=b[1],l=a(h[4],m[9]),n=c(h[8],l,k),p=a(h[4],m[13]),q=c(h[8],p,j),r=a(h[18],ai),s=a(h[4],r),g=c(h[8],s,i);return function(b){var
a=g?g[1]:0;return e(M[4],n,q,a)}}}}return a(o[2],jp)}],jo];function
jr(b,a){return e(b4[1],a[1],[0,js,b],a[2])}c(aj[87],jr,jq);var
jt=0,jv=[0,function(b){return b?a(o[2],ju):function(a){return ak[5]}},jt],jx=[0,function(b){if(b){var
c=b[2];if(c){var
d=c[2];if(d)if(!d[2])return function(a){return ak[6]}}}return a(o[2],jw)},jv];function
jy(b,a){return c(ak[3],[0,jz,b],a)}c(aj[87],jy,jx);var
jB=[5,[6,a(p[12],ai)]],jC=a(h[18],ai),jD=[0,[0,a(h[4],jC)],jB],jE=[0,[1,c(l[10],0,jD)],0],jF=[6,a(p[12],m[13])],jG=[0,[0,a(h[4],m[13])],jF],jI=[0,jH,[0,[1,c(l[10],0,jG)],jE]],jJ=[6,a(p[12],m[9])],jK=[0,[0,a(h[4],m[9])],jJ],jN=[0,[0,jM,[0,jL,[0,[1,c(l[10],0,jK)],jI]]],jA];function
jO(b,a){return e(b5[1],[0,jP,b],0,a)}c(aj[87],jO,jN);var
jQ=0,jS=[0,function(b){if(b){var
d=b[2];if(d){var
e=d[2];if(e)if(!e[2]){var
g=e[1],i=d[1],k=b[1],l=a(h[6],f[1][1]),n=c(f[12][2][7],l,k),p=a(h[17],m[13]),q=a(h[6],p),r=c(f[12][2][7],q,i),s=a(h[17],m[13]),t=a(h[6],s),u=c(f[12][2][7],t,g);return function(c){var
b=a(j[17][cn],u);return O(M[6],n,r,b[2],b[1])}}}}return a(o[2],jR)},jQ],jT=a(j[19][12],jS);e(f[6][9],0,[0,E,jU],jT);function
jV(t){var
k=[0,a(i[1][7],jW)],b=m[13],h=0,j=0;if(0===b[0]){var
n=[0,jY,[0,[1,c(l[10],0,[0,[0,[5,[0,b[1]]]],k])],j]],o=[0,a(i[1][7],jZ)],d=m[13];if(0===d[0]){var
p=[0,j1,[0,[1,c(l[10],0,[0,[2,[5,[0,d[1]]]],o])],n]],q=[0,a(i[1][7],j2)],g=f[1][1],r=0;if(0===g[0]){var
s=[0,[0,j4,[0,[1,c(l[10],0,[0,[6,[0,g[1]],r],q])],p]],h];return e(f[9][4],[0,E,j5],0,s)}throw[0,F,j3]}throw[0,F,j0]}throw[0,F,jX]}c(aq[19],jV,E);function
a$(b){if(0===b[0])return aM(b[1]);var
e=c(d[31],N[23],b[1]),f=a(d[3],j6);return c(d[12],f,e)}var
ba=a(h[3],j7),j8=a(h[4],ba),bb=e(p[13],p[9],j9,j8),j_=0,j$=0,ka=[0,[0,[0,0,[6,aN]],function(a,b){return[0,a]}],j$];function
kb(a,c,b){return[1,a]}var
kc=[6,p[15][1]],ke=[0,0,[0,[0,0,0,[0,[0,[0,[0,0,[0,a(n[11],kd)]],kc],kb],ka]],j_]];e(p[22],bb,0,ke);function
kf(h,g,f,c){var
b=a(d[3],kg);return e(z[3],0,0,b)}function
kh(h,g,f,c){var
b=a(d[3],ki);return e(z[3],0,0,b)}function
kj(c,b,a){return a$}O(f[2][1],ba,kj,kh,kf);function
b6(b){var
c=e(d[38],d[28],a$,b);return a(d[45],c)}var
al=a(h[3],kk),kl=a(h[4],al),b7=e(p[13],p[9],km,kl),kn=0,ko=0;function
kp(d,a,c,b){return a}var
kr=[0,a(n[11],kq)],kt=[2,[6,bb],[0,a(n[11],ks)]],kv=[0,0,[0,[0,0,0,[0,[0,[0,[0,[0,0,[0,a(n[11],ku)]],kt],kr],kp],ko]],kn]];e(p[22],b7,0,kv);function
kw(h,g,f,c){var
b=a(d[3],kx);return e(z[3],0,0,b)}function
ky(h,g,f,c){var
b=a(d[3],kz);return e(z[3],0,0,b)}function
kA(c,b,a){return b6}O(f[2][1],al,kA,ky,kw);var
kB=0,kG=[0,[0,0,function(b){return b?a(o[2],kC):function(g){var
b=a(d[22],kD);c(ah[7],0,b);var
e=M[8][1];function
f(e,b){var
f=a(ap[8],b[2]),g=a(d[3],kE),h=a(d[13],0),i=a(ap[8],b[1]),j=a(d[3],kF),k=a(d[13],0),l=a(T[20],e),m=a(N[12],l),n=c(d[12],m,k),o=c(d[12],n,j),p=c(d[12],o,i),q=c(d[12],p,h),r=c(d[12],q,g),s=c(d[12],r,f),t=c(d[26],2,s);return c(ah[7],0,t)}return c(T[24][10],f,e)}}],kB],kI=[0,[0,0,function(b){if(b){var
d=b[2];if(d){var
f=d[2];if(f)if(!f[2]){var
i=f[1],j=d[1],k=b[1],l=a(h[4],m[9]),n=c(h[8],l,k),p=a(h[4],m[13]),q=c(h[8],p,j),r=a(h[18],al),s=a(h[4],r),g=c(h[8],s,i);return function(b){var
a=g?g[1]:0;return e(M[7],n,q,a)}}}}return a(o[2],kH)}],kG];function
kJ(b,a){return e(b4[1],a[1],[0,kK,b],a[2])}c(aj[87],kJ,kI);var
kL=0,kN=[0,function(b){return b?a(o[2],kM):function(a){return ak[5]}},kL],kP=[0,function(b){if(b){var
c=b[2];if(c){var
d=c[2];if(d)if(!d[2])return function(a){return ak[6]}}}return a(o[2],kO)},kN];function
kQ(b,a){return c(ak[3],[0,kR,b],a)}c(aj[87],kQ,kP);var
kT=[5,[6,a(p[12],al)]],kU=a(h[18],al),kV=[0,[0,a(h[4],kU)],kT],kW=[0,[1,c(l[10],0,kV)],0],kX=[6,a(p[12],m[13])],kY=[0,[0,a(h[4],m[13])],kX],k0=[0,kZ,[0,[1,c(l[10],0,kY)],kW]],k1=[6,a(p[12],m[9])],k2=[0,[0,a(h[4],m[9])],k1],k5=[0,[0,k4,[0,k3,[0,[1,c(l[10],0,k2)],k0]]],kS];function
k6(b,a){return e(b5[1],[0,k7,b],0,a)}c(aj[87],k6,k5);var
k8=0,k_=[0,function(b){if(b){var
d=b[2];if(d){var
e=d[2];if(e)if(!e[2]){var
g=e[1],i=d[1],k=b[1],l=a(h[6],f[1][1]),n=c(f[12][2][7],l,k),p=a(h[17],m[13]),q=a(h[6],p),r=c(f[12][2][7],q,i),s=a(h[17],m[13]),t=a(h[6],s),u=c(f[12][2][7],t,g);return function(c){var
b=a(j[17][cn],u);return O(M[9],n,r,b[2],b[1])}}}}return a(o[2],k9)},k8],k$=a(j[19][12],k_);e(f[6][9],0,[0,E,la],k$);function
lb(s){var
k=[0,a(i[1][7],lc)],b=m[13],h=0,j=0;if(0===b[0]){var
n=[0,le,[0,[1,c(l[10],0,[0,[0,[5,[0,b[1]]]],k])],j]],o=[0,a(i[1][7],lf)],d=m[13];if(0===d[0]){var
p=[0,lh,[0,[1,c(l[10],0,[0,[2,[5,[0,d[1]]]],o])],n]],q=[0,a(i[1][7],li)],g=f[1][1];if(0===g[0]){var
r=[0,[0,lk,[0,[1,c(l[10],0,[0,[5,[0,g[1]]],q])],p]],h];return e(f[9][4],[0,E,ll],0,r)}throw[0,F,lj]}throw[0,F,lg]}throw[0,F,ld]}c(aq[19],lb,E);var
b8=[0,E,aM,a_,aN,b2,ai,b3,a$,ba,bb,b6,al,b7];bc(375,b8,"Newring_plugin.G_newring");bc(cv,[0,M,b8],"Newring_plugin");return});
