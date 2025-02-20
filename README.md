# benthic-fauna-index
Calculating benthic fauna indices and EQR values


https://benthicfauna.p.niva.no/


# benthic fauna app deployment

Private deployment for the public repository [benthic-fauna-index](https://github.com/NIVANorge/benthic-fauna-index). 

You have to manually select the tag of the image, which is automatically deployed when the tag change is committed on github.  

1. Select your image tag from [artifact registry](https://console.cloud.google.com/artifacts/docker/niva-cd/europe-west1/images/benthicfauna?project=niva-cd)
2. Update the newTag field for benthic-fauna-index in  [kustomization.yaml](https://github.com/NIVANorge/nivacloud-manifests/blob/main/workloads/benthicfauna/base/kustomization.yaml) in the [nivacloud-mainifests repository](https://github.com/NIVANorge/nivacloud-manifests)

3. Commit and push
