process miminap {

    input:
    path fastq
    val sample_id
    
    output:
    path "mapped_${sample_id}.${params.extenssion}"

    script:
    """
    minimap2 -ax map-ont -t ${task.cpu} ${params.reference_genome} ${fastq} | samtools sort -@ ${task.cpus} -O ${params.extenssion} -o mapped_${sample_id}.${params.extenssion}
    """
}
