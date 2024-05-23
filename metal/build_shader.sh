xcrun -sdk macosx metal -c src/shader.metal -o src/shader.air
xcrun -sdk macosx metallib src/shader.air -o src/shader.metallib