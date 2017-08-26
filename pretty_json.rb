f = File.open(ARGV[0]).read
File.open("#{ARGV[0]}", 'w') do |fi|
    fi.write(f.gsub('},{', "},\n\t\t{").gsub('],', "],\n").gsub(':[{', ":[\n\t\t{").gsub('}],', "}\n\t],\t"))
end