import os

# Directorio donde se encuentran los archivos
directorio = "c:\\Users\\Usuario\\Documents\\Universidad\\austral\\2024\\lab1\\TareasHogar\\Tarea-05\\exp-ec01\\ZZ0001-e001\\"

# Listar archivos que empiecen con "ZZ0001"
archivos_zz0001 = [archivo for archivo in os.listdir(directorio) if archivo.startswith("ZZ0001")]

# Ruta del archivo de texto donde se guardarán los nombres
ruta_archivo = "c:\\Users\\Usuario\\Documents\\Universidad\\austral\\2024\\lab1\\TareasHogar\\Tarea-05\\archivos.txt"

# Escribir los nombres de los archivos en el archivo de texto
with open(ruta_archivo, "w") as archivo:
    for nombre_archivo in archivos_zz0001:
        archivo.write(nombre_archivo + "\n")

print("Archivos guardados en:", ruta_archivo)
