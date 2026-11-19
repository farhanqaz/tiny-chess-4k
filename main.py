import json,os,urllib.request as ur
P='PNBRQK'
D=[(1,0),(-1,0),(0,1),(0,-1),(1,1),(1,-1),(-1,1),(-1,-1)]
N=[(1,2),(2,1),(-1,2),(-2,1),(1,-2),(2,-1),(-1,-2),(-2,-1)]
V=[100,320,330,500,900,0]
T=[(ord(c)-75)*5 for c in'KKKKKKKKUUUUUUUUMMOQQOMMLLMPPMLLKKKOOKKKLJIKKIJLLMMGGMMLKKKKKKKKACEEEECACGKKKKGCEKMNNMKEELNOONLEEKNOONKEELMNNMLECGKLLKGCACEEEECAGIIIIIIGIKKKKKKIIKLMMLKIILLMMLLIIKMMMMKIIMMMMMMIILKKKKLIGIIIIIIGKKKKKKKKLMMMMMMLJKKKKKKJJKKKKKKJJKKKKKKJJKKKKKKJJKKKKKKJKKKLLKKKGIIJJIIGIKKKKKKIIKLLLLKIJKLLLLKJKKLLLLKJILLLLLKIIKLKKKKIGIIJJIIGECCAACCEECCAACCEECCAACCEECCAACCEGEECCEEGIGGGGGGIOOKKKKOOOQMKKMQO']
R={0:'q',4:'kq',7:'k',56:'Q',60:'KQ',63:'K'}
I=10**9;L=5000;n=0;K={}
class Z(Exception):pass
def A(b,s,o):
 r,f=divmod(s,8);p=P if o else P.lower();d=1 if o else -1;c=lambda x,y:b[8*x+y]if 0<=x<8 and 0<=y<8 else'.'
 if p[0]in(c(r+d,f-1),c(r+d,f+1))or any(c(r+x,f+y)==p[1]for x,y in N)or any(c(r+x,f+y)==p[5]for x,y in D):return 1
 for x,y in D:
  i,j=r+x,f+y
  while 0<=i<8 and 0<=j<8 and b[8*i+j]=='.':i+=x;j+=y
  if c(i,j)in(p[4],p[3]if x*y==0 else p[2]):return 1
 return 0
def M(p,m):
 b,w,c,e=p;s,t,z=m;b=b[:];x=b[s];q=-1
 if x in'Pp'and s%8!=t%8 and b[t]=='.':b[t+8 if w else t-8]='.'
 if x in'Kk'and abs(t-s)==2:b[t-1 if t>s else t+1]=b[t+1 if t>s else t-2];b[t+1 if t>s else t-2]='.'
 if x in'Pp'and abs(t-s)==16:q=(s+t)//2
 b[t]=(z.upper()if w else z)if z else x;b[s]='.'
 c=''.join(y for y in c if y not in R.get(s,'')+R.get(t,''))
 return b,not w,c,q
def G(p):
 b,w,c,e=p;o=not w;m=[]
 for s,x in enumerate(b):
  if x=='.'or x.isupper()!=w:continue
  r,f=divmod(s,8);k=x.lower();z=[]
  if k=='p':
   d=-1 if w else 1;t=s+8*d
   if b[t]=='.':z+=[t]+[t+8*d]*(r==(6 if w else 1)and b[t+8*d]=='.')
   z+=[t+g for g in(-1,1)if 0<=f+g<8 and(b[t+g]!='.'and b[t+g].isupper()!=w or t+g==e)]
   m+=[(s,t,q)for t in z for q in('qrbn'if t<8 or t>55 else[''])]
  else:
   for dx,dy in{'n':N,'k':D,'r':D[:4],'b':D[4:]}.get(k,D):
    i,j=r+dx,f+dy
    while 0<=i<8 and 0<=j<8:
     t=8*i+j
     if b[t]!='.'and b[t].isupper()==w:break
     m.append((s,t,''))
     if b[t]!='.'or k in'nk':break
     i+=dx;j+=dy
  if k=='k'and s==(60 if w else 4):
   R_='R'if w else'r'
   if('K'if w else'k')in c and''.join(b[s+1:s+4])=='..'+R_ and not any(A(b,t,o)for t in(s,s+1,s+2)):m.append((s,s+2,''))
   if('Q'if w else'q')in c and''.join(b[s-4:s])==R_+'...'and not any(A(b,t,o)for t in(s,s-1,s-2)):m.append((s,s-2,''))
 return[m for m in m if not A(b:=M(p,m)[0],b.index('K'if w else'k'),o)]
def E(p):
 b,w=p[0],p[1];v=30*((b.count('B')>1)-(b.count('b')>1))
 for i,x in enumerate(b):
  if x!='.':k=P.index(x.upper());g=1 if x.isupper()else-1;v+=g*(V[k]+T[64*k+(i if g>0 else i^56)])
 return v if w else-v
def Y(b,m,kl):
 s,t,z=m;x=b[s];v=b[t]
 if v!='.'or x in'Pp'and s%8!=t%8:return 10000+10*(V[P.find(v.upper())]if v!='.'else 100)-P.index(x.upper())
 if z:return 9000
 return 8000 if m in kl else 0
def Q(p,d,a,b,y,q):
 global n
 n+=1
 if n>L:raise Z
 w=p[1];bd=p[0];ms=G(p);ck=A(bd,bd.index('K'if w else'k'),not w)
 if not ms:return -100000+y if ck else 0
 st=None
 if d<=0:
  if not ck:
   st=E(p)
   if q>=4 or st>=b:return st
   a=max(a,st);ms=[m for m in ms if bd[m[1]]!='.'or m[2]or m[1]==p[3]and bd[m[0]]in'Pp']
  elif q>=4:return E(p)
 kl=K.setdefault(y,[])
 best=-I if st is None else st
 for m in sorted(ms,key=lambda m:-Y(bd,m,kl)):
  best=max(best,-Q(M(p,m),d-1 if d>0 else 0,-b,-a,y+1,q+(d<=0)))
  a=max(a,best)
  if a>=b:
   if bd[m[1]]=='.'and not m[2]and m not in kl:kl[:]=([m]+kl)[:2]
   break
 return best
def pk(p):
 global n,K
 n=0;K={};bm=None
 try:
  for d in range(1,6):
   ms=G(p);a=-I;nb=None
   for m in sorted(ms,key=lambda m:-Y(p[0],m,[])):
    v=-Q(M(p,m),d-1,-I,-a+1,1,0)
    if v>a or v==a and ms.index(m)<ms.index(nb):a=v;nb=m
   bm=nb
 except Z:pass
 return bm
def F(f):
 a,w,c,e=f.split()[:4];b=[]
 for h in a:b+=['.']*int(h)if h.isdigit()else[h]*(h!='/')
 return b,w=='w',c,-1 if e=='-'else 8*(8-int(e[1]))+'abcdefgh'.index(e[0])
if __name__=='__main__':
 U=os.environ.get('MAKEFUN_ENV_URL','')
 def po(p,d):return json.load(ur.urlopen(ur.Request(U+p,json.dumps(d).encode(),{'Content-Type':'application/json'})))
 try:
  g=po('/game',{})
  while g['status']=='your_move'and g['legal_moves']:
   m=pk(F(g['fen']))
   u=''.join('abcdefgh'[x%8]+str(8-x//8)for x in m[:2])+m[2]if m else g['legal_moves'][0]
   g=po('/move',{'game':g['game'],'move':u})
 except(OSError,ValueError,KeyError):pass
