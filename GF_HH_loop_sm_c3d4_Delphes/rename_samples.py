# Script to move and rename delphes samples


import os, glob


outdir = 'GF_HH_SM_c3_0_d4_0_14TeV'
if not os.path.isdir(outdir):
    os.mkdir(outdir)

samples = glob.glob('Events/run_*_decayed_1/tag_1_delphes_events.root')

for i in range(len(samples)):
    cmd = 'cp %s %s/sample_%d.root'%(samples[i],outdir,i)
    print(cmd)
    os.system(cmd)


cmd_tar = 'tar -cf %s.tar %s'%(outdir,outdir)
print(cmd_tar)
os.system(cmd_tar)





