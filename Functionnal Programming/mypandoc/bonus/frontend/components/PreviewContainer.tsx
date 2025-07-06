'use client';

import React, { useState } from 'react';
import { FileInfo } from './FileConverterApp';
import { Tabs, TabsContent, TabsList, TabsTrigger } from '@/components/ui/tabs';
import { Button } from '@/components/ui/button';
import { Download, Copy, Check } from 'lucide-react';
import CodePreview from './previews/CodePreview';
import MarkdownPreview from './previews/MarkdownPreview';

interface PreviewContainerProps {
  file: FileInfo;
  targetFormat?: string;
}

export default function PreviewContainer({ file, targetFormat }: PreviewContainerProps) {
  const [copied, setCopied] = useState(false);
  
  const handleCopyContent = async () => {
    await navigator.clipboard.writeText(file.content);
    setCopied(true);
    setTimeout(() => setCopied(false), 2000);
  };
  
  const handleDownload = () => {
    const blob = new Blob([file.content], { type: 'text/plain' });
    const url = URL.createObjectURL(blob);
    const a = document.createElement('a');
    a.href = url;
    a.download = "file." + (targetFormat ? targetFormat : file.type); 
    document.body.appendChild(a);
    a.click();
    document.body.removeChild(a);
    URL.revokeObjectURL(url);
  };
  
  return (
    <div className="space-y-3">
      <div className="flex justify-between items-center">
        <div className="flex items-center gap-2">
          <span className="font-medium truncate max-w-[200px]">{targetFormat ? targetFormat.toUpperCase() : file.type.toUpperCase()}</span>
        </div>
        
        <div className="flex gap-2">
          <Button
            variant="outline"
            size="sm"
            onClick={handleCopyContent}
            className="h-8 px-2"
          >
            {copied ? <Check className="h-4 w-4" /> : <Copy className="h-4 w-4" />}
            <span className="sr-only md:not-sr-only md:ml-2">
              {copied ? 'Copied' : 'Copy'}
            </span>
          </Button>
          
          <Button
            variant="outline"
            size="sm"
            onClick={handleDownload}
            className="h-8 px-2"
          >
            <Download className="h-4 w-4" />
            <span className="sr-only md:not-sr-only md:ml-2">Download</span>
          </Button>
        </div>
      </div>
      
      {(targetFormat ? targetFormat === 'markdown' : file.type === 'markdown') ? (
        <Tabs defaultValue="preview">
          <TabsList className="grid w-full grid-cols-2">
            <TabsTrigger value="preview">Preview</TabsTrigger>
            <TabsTrigger value="code">Code</TabsTrigger>
          </TabsList>
          
          <TabsContent value="preview" className="mt-2">
            <div className="border rounded-md p-4 bg-card max-h-[400px] overflow-auto">
              <MarkdownPreview content={file.content} />
            </div>
          </TabsContent>
          
          <TabsContent value="code" className="mt-2">
            <div className="border rounded-md overflow-hidden max-h-[400px]">
              <CodePreview content={file.content} language="markdown" />
            </div>
          </TabsContent>
        </Tabs>
      ) : (
        <div className="border rounded-md p-4 bg-card max-h-[400px] overflow-auto">
          <CodePreview 
            content={file.content} 
            language={file.type === 'unknown' ? 'plaintext' : file.type} 
          />
        </div>
      )}
    </div>
  );
}