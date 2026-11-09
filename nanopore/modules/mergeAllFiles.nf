process mergeAllFiles {

    input:
    tuple val(sample_id), path(files)

    output:
    path "merged_${sample_id}.fastq.gz", emit: merged
    path "merge_order_${sample_id}.txt", emit: order
    val  "${sample_id}", emit: id

    script:
    """
    for f in ${files}; do
        echo "\$f" >> merge_order_${sample_id}.txt
    done
    cat ${files} > merged_${sample_id}.fastq.gz
    """
}

