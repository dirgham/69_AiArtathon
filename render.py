# python code to render
# Run as: blender -b <filename> -P <this_script> -- <image_path>
import bpy, sys, os

# Assume the last argument is image path
imagePath = sys.argv[-1]

if os.path.exists(imagePath):
    # Assume object, material and texture name (and settings) are valid
    charObj = bpy.data.objects['Sphere']
    charMat = charObj.material_slots['Material.007'].material
    charTex = charMat.texture_slots['Texture'].texture
    charTex.image.filepath = imagePath

    # Render to separate file, identified by texture file
    imageBaseName = bpy.path.basename(imagePath)
    bpy.context.scene.render.filepath += '-' + imageBaseName

    # Render still image, automatically write to output path
    bpy.ops.render.render(write_still=True)
else:
    print("Missing Image:", imagePath)