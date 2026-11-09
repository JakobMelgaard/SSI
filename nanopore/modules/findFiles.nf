process findFiles {

    input:
    path dir

    output:
    path "found_files.txt"

    script:
    """
    find -L ${dir} -type f -name "*pass*.fastq.gz" | while read -r f; do
        base=\$(basename "\$f")
        sample_id=\$(echo "\$base" | sed -E 's/.*_pass_([^_]+)_.*/\\1/')
        echo -e "\${sample_id}\t\${f}"
    done > found_files.txt
    """
}

