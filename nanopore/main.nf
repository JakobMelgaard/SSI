include {findFiles} from './modules/findFiles.nf'
include {mergeAllFiles} from './modules/mergeAllFiles.nf'
include {miminap} from './modules/miminap.nf'



workflow {

    main:
        findFiles(params.dir)

        grouped_ch = findFiles.out
            .splitText()
            .map { it.trim() }
            .filter { it }
            .map { line ->
                def (sample_id, path_str) = line.split('\t')
                tuple(sample_id, file(path_str))
            }
            .groupTuple()
            .map { sample_id, files ->
                def sorted_files = files.sort { f ->
                    (f.name =~ /_(\d+)\.fastq\.gz$/)[0][1] as Integer
                }
                tuple(sample_id, sorted_files)
            }

        mergeAllFiles(grouped_ch)
	
	miminap(mergeAllFiles.out.merged,mergeAllFiles.out.id)

    publish:
        order = mergeAllFiles.out.order
        merge = mergeAllFiles.out.merged
        align = miminap.out

}

output {
    order{
        path "."
        mode "copy"
    }
    merge{
        path "."
        mode "copy"
    }
    align{
		path "."
		mode "copy"
    }
}



