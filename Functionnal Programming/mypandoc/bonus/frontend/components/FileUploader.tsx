'use client';

import { useState, useRef } from 'react';
import { FileTextIcon, Upload } from 'lucide-react';
import { Button } from '@/components/ui/button';
import { cn } from '@/lib/utils';
import { FileInfo } from './FileConverterApp';

interface FileUploaderProps {
  onFileUpload: (file: FileInfo) => void;
}

export default function FileUploader({ onFileUpload }: FileUploaderProps) {
  const [isDragging, setIsDragging] = useState(false);
  const [isLoading, setIsLoading] = useState(false);
  const fileInputRef = useRef<HTMLInputElement>(null);

  const handleDragOver = (e: React.DragEvent<HTMLDivElement>) => {
    e.preventDefault();
    setIsDragging(true);
  };

  const handleDragLeave = (e: React.DragEvent<HTMLDivElement>) => {
    e.preventDefault();
    setIsDragging(false);
  };

  const handleDrop = (e: React.DragEvent<HTMLDivElement>) => {
    e.preventDefault();
    setIsDragging(false);
    
    if (e.dataTransfer.files.length > 0) {
      processFile(e.dataTransfer.files[0]);
    }
  };

  const handleFileInputChange = (e: React.ChangeEvent<HTMLInputElement>) => {
    if (e.target.files && e.target.files.length > 0) {
      processFile(e.target.files[0]);
    }
  };

  const processFile = async (file: File) => {
    setIsLoading(true);
    
    try {
      const content = await readFileContent(file);
      const fileType = determineFileType(file.name);
      
      onFileUpload({
        content,
        type: fileType,
      });
    } catch (error) {
      console.error('Error reading file:', error);
    } finally {
      setIsLoading(false);
    }
  };

  const readFileContent = (file: File): Promise<string> => {
    return new Promise((resolve, reject) => {
      const reader = new FileReader();
      reader.onload = (e) => resolve(e.target?.result as string);
      reader.onerror = (e) => reject(e);
      reader.readAsText(file);
    });
  };

  const determineFileType = (fileName: string): 'json' | 'xml' | 'markdown' | 'unknown' => {
    const extension = fileName.split('.').pop()?.toLowerCase();
    
    switch (extension) {
      case 'json':
        return 'json';
      case 'xml':
        return 'xml';
      case 'md':
      case 'markdown':
        return 'markdown';
      default:
        return 'unknown';
    }
  };

  return (
    <div>
      <div
        className={cn(
          "border-2 border-dashed rounded-lg p-8 text-center transition-colors",
          isDragging 
            ? "border-primary bg-primary/5" 
            : "border-muted-foreground/25 hover:border-primary/50",
          "cursor-pointer"
        )}
        onDragOver={handleDragOver}
        onDragLeave={handleDragLeave}
        onDrop={handleDrop}
        onClick={() => fileInputRef.current?.click()}
      >
        <div className="flex flex-col items-center justify-center space-y-4">
          <div className="rounded-full bg-muted p-3">
            <Upload className="h-8 w-8 text-muted-foreground" />
          </div>
          <div className="space-y-2">
            <h3 className="text-lg font-semibold">Upload a file</h3>
            <p className="text-sm text-muted-foreground max-w-xs mx-auto">
              Drag and drop or click to upload JSON, XML, or Markdown files
            </p>
          </div>
          
          <div className="flex gap-2 flex-wrap justify-center">
            <span className="inline-flex items-center gap-1 px-3 py-1 rounded-full text-xs font-medium bg-blue-100 text-blue-800 dark:bg-blue-900/30 dark:text-blue-300">
              <FileTextIcon className="h-3 w-3" /> .json
            </span>
            <span className="inline-flex items-center gap-1 px-3 py-1 rounded-full text-xs font-medium bg-green-100 text-green-800 dark:bg-green-900/30 dark:text-green-300">
              <FileTextIcon className="h-3 w-3" /> .xml
            </span>
            <span className="inline-flex items-center gap-1 px-3 py-1 rounded-full text-xs font-medium bg-purple-100 text-purple-800 dark:bg-purple-900/30 dark:text-purple-300">
              <FileTextIcon className="h-3 w-3" /> .md
            </span>
          </div>
        </div>
      </div>
      
      <input
        type="file"
        className="hidden"
        ref={fileInputRef}
        onChange={handleFileInputChange}
        accept=".json,.xml,.md,.markdown"
      />
      
      <div className="mt-4 flex justify-center">
        <Button
          onClick={() => fileInputRef.current?.click()}
          disabled={isLoading}
          className="relative overflow-hidden transition-all"
        >
          {isLoading ? 'Loading...' : 'Select File'}
        </Button>
      </div>
    </div>
  );
}